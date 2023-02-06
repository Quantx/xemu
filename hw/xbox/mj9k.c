/*
 * QEMU Mega Jockey 9000 (Steel Battalion) Controller
 *
 * Copyright (c) 2023 Samuel Deutsch
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "qemu/osdep.h"
#include "hw/qdev-properties.h"
#include "migration/vmstate.h"
#include "sysemu/sysemu.h"
#include "hw/hw.h"
#include "ui/console.h"
#include "hw/usb.h"
#include "hw/usb/desc.h"
#include "ui/xemu-input.h"

#define DEBUG_MJ9K
#ifdef DEBUG_MJ9K
#define DPRINTF printf
#else
#define DPRINTF(...)
#endif

/*
 * https://xboxdevwiki.net/Xbox_Input_Devices#bType_.3D_128:_Steel_Battalion
 * https://xboxdevwiki.net/Xbox_Input_Devices#Steel_Battalion_Controller
 */

#define USB_CLASS_XID  0x58
#define USB_DT_XID     0x42

#define HID_GET_REPORT       0x01
#define HID_SET_REPORT       0x09
#define XID_GET_CAPABILITIES 0x01

#define TYPE_USB_MJ9K "usb-mj9k-gamepad"
#define MJ9K_DESCRIPTION "Capcom Mega Jockey 9000 Controller"
#define USB_XID(obj) OBJECT_CHECK(USBXIDState, (obj), TYPE_USB_MJ9K)

enum {
    STR_MANUFACTURER = 1,
    STR_PRODUCT,
    STR_SERIALNUMBER,
};

static const USBDescStrings desc_strings = {
    [STR_MANUFACTURER] = "QEMU",
    [STR_PRODUCT]      = MJ9K_DESCRIPTION,
    [STR_SERIALNUMBER] = "1",
};
 
typedef struct XIDDesc {
    uint8_t  bLength;
    uint8_t  bDescriptorType;
    uint16_t bcdXid;
    uint8_t  bType;
    uint8_t  bSubType;
    uint8_t  bMaxInputReportSize;
    uint8_t  bMaxOutputReportSize;
    uint16_t wAlternateProductIds[4];
} QEMU_PACKED XIDDesc;
 
typedef struct MJ9KGamepadReport { // 26 bytes
    uint8_t  bReportId;
    uint8_t  bLength;
    uint8_t  bButtons[5];
    uint8_t _pad;
    uint16_t wAimX;
    uint16_t wAimY;
    int16_t  sRotation;
    int16_t  sSightX;
    int16_t  sSightY;
    uint16_t wClutch;
    uint16_t wBrake;
    uint16_t wThrottle;
    uint8_t  bTuner;
    uint8_t  bShifter;
} QEMU_PACKED MJ9KGamepadReport;
 
typedef struct MJ9KGamepadOutputReport { // 22 bytes
    uint8_t  bReportId;
    uint8_t  bLength;
    uint8_t  bLights[20];
} QEMU_PACKED MJ9KGamepadOutputReport;

typedef struct USBXIDState {
    USBDevice               dev;
    USBEndpoint             *intr;
    const XIDDesc           *xid_desc;
    MJ9KGamepadReport       in_state;
    MJ9KGamepadReport       in_state_capabilities;
    MJ9KGamepadOutputReport out_state;
    MJ9KGamepadOutputReport out_state_capabilities;
    uint8_t                 device_index;
} USBXIDState;

static const USBDescIface desc_iface_mj9k_gamepad = {
    .bInterfaceNumber              = 0,
    .bNumEndpoints                 = 2,
    .bInterfaceClass               = USB_CLASS_XID,
    .bInterfaceSubClass            = 0x42,
    .bInterfaceProtocol            = 0x00,
    .eps = (USBDescEndpoint[]) {
        {
            .bEndpointAddress      = USB_DIR_IN | 0x02,
            .bmAttributes          = USB_ENDPOINT_XFER_INT,
            .wMaxPacketSize        = 0x20,
            .bInterval             = 4,
        },
        {
            .bEndpointAddress      = USB_DIR_OUT | 0x01,
            .bmAttributes          = USB_ENDPOINT_XFER_INT,
            .wMaxPacketSize        = 0x20,
            .bInterval             = 4,
        },
    },
};

static const USBDescDevice desc_device_mj9k_gamepad = {
    .bcdUSB                        = 0x0110,
    .bMaxPacketSize0               = 8,
    .bNumConfigurations            = 1,
    .confs = (USBDescConfig[]) {
        {
            .bNumInterfaces        = 1,
            .bConfigurationValue   = 1,
            .bmAttributes          = USB_CFG_ATT_ONE,
            .bMaxPower             = 50,
            .nif = 1,
            .ifs = &desc_iface_mj9k_gamepad,
        },
    },
};

static const USBDesc desc_mj9k_gamepad = {
    .id = {
        .idVendor          = 0x0a7b,
        .idProduct         = 0xd000,
        .bcdDevice         = 0x0100,
        .iManufacturer     = STR_MANUFACTURER,
        .iProduct          = STR_PRODUCT,
        .iSerialNumber     = STR_SERIALNUMBER,
    },
    .full = &desc_device_mj9k_gamepad,
    .str  = desc_strings,
};

static const XIDDesc desc_xid_mj9k_gamepad = {
    .bLength              = 0x10,
    .bDescriptorType      = USB_DT_XID,
    .bcdXid               = 0x100,
    .bType                = 0x80,
    .bSubType             = 0x01,
    .bMaxInputReportSize  = 0x1A,
    .bMaxOutputReportSize = 0x16,
    .wAlternateProductIds = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF },
};

static void update_output(USBXIDState *s)
{
    if (xemu_input_get_test_mode()) {
        // Don't report changes if we are testing the controller while running
        return;
    }
    
    // TODO: Maybe have some kind of on screen indicators for the lights?
}

static void update_input(USBXIDState *s)
{
    if (xemu_input_get_test_mode()) {
        // Don't report changes if we are testing the controller while running
        return;
    }
    
    ControllerState *state = xemu_input_get_bound(s->device_index);
    assert(state);
    xemu_input_update_controller(state);
    
    // Main Weapon Button starts the game
    s->in_state.bButtons[0] = 0;
    if (state->buttons & CONTROLLER_BUTTON_START) {
        s->in_state.bButtons[0] |= 0x01;
    }
    if (state->buttons & CONTROLLER_BUTTON_BACK) {
        s->in_state.bButtons[0] |= 0x04;
    }
    
    s->in_state.wAimX = (uint32_t)(state->axis[CONTROLLER_AXIS_RSTICK_X]) + 0x8000;
    s->in_state.wAimY = (uint32_t)(state->axis[CONTROLLER_AXIS_RSTICK_Y]) + 0x8000;
    s->in_state.sRotation = state->axis[CONTROLLER_AXIS_LSTICK_X];
    
    s->in_state.sSightX = 0;
    s->in_state.sSightY = 0;
    if (state->buttons & CONTROLLER_BUTTON_DPAD_DOWN) {
        s->in_state.sSightY = 0x7FFF; // int16_t max
    }
    if (state->buttons & CONTROLLER_BUTTON_DPAD_UP) {
        s->in_state.sSightY = 0x8000; // int16_t min
    }
    if (state->buttons & CONTROLLER_BUTTON_DPAD_RIGHT) {
        s->in_state.sSightX = 0x7FFF; // int16_t max
    }
    if (state->buttons & CONTROLLER_BUTTON_DPAD_LEFT) {
        s->in_state.sSightX = 0x8000; // int16_t min
    }
}

static void usb_xid_handle_reset(USBDevice *dev)
{
    DPRINTF("xid reset\n");
}

static void usb_xid_handle_control(USBDevice *dev, USBPacket *p,
               int request, int value, int index, int length, uint8_t *data)
{
    USBXIDState *s = (USBXIDState *)dev;

    DPRINTF("xid handle_control 0x%x 0x%x\n", request, value);

    int ret = usb_desc_handle_control(dev, p, request, value, index, length, data);
    if (ret >= 0) {
        DPRINTF("xid handled by usb_desc_handle_control: %d\n", ret);
        return;
    }

    switch (request) {
    /* HID requests */
    case ClassInterfaceRequest | HID_GET_REPORT:
        DPRINTF("xid GET_REPORT 0x%x\n", value);
        update_input(s);
        if (value == 0x0100) { /* input */
            if (length <= s->in_state.bLength) {
                memcpy(data, &s->in_state, s->in_state.bLength);
                p->actual_length = length;
            } else {
                p->status = USB_RET_STALL;
            }
        } else {
            p->status = USB_RET_STALL;
            assert(false);
        }
        break;
    case ClassInterfaceOutRequest | HID_SET_REPORT:
        DPRINTF("xid SET_REPORT 0x%x\n", value);
        if (value == 0x0200) { /* output */
            /* Read length, then the entire packet */
            if (length == s->out_state.bLength) {
                memcpy(&s->out_state, data, sizeof(s->out_state));

                /* FIXME: This should also be a STALL */
                assert(s->out_state.bLength == sizeof(s->out_state));

                p->actual_length = length;
            } else {
                p->status = USB_RET_STALL;
            }
            update_output(s);
        } else {
            p->status = USB_RET_STALL;
            assert(false);
        }
        break;
    /* XID requests */
    case VendorInterfaceRequest | USB_REQ_GET_DESCRIPTOR:
        DPRINTF("xid GET_DESCRIPTOR 0x%x\n", value);
        if (value == 0x4200) {
            assert(s->xid_desc->bLength <= length);
            memcpy(data, s->xid_desc, s->xid_desc->bLength);
            p->actual_length = s->xid_desc->bLength;
        } else {
            p->status = USB_RET_STALL;
            assert(false);
        }
        break;
    case VendorInterfaceRequest | XID_GET_CAPABILITIES:
        DPRINTF("xid XID_GET_CAPABILITIES 0x%x\n", value);
        if (value == 0x0100) {
            if (length > s->in_state_capabilities.bLength) {
                length = s->in_state_capabilities.bLength;
            }
            memcpy(data, &s->in_state_capabilities, length);
            p->actual_length = length;
        } else if (value == 0x0200) {
            if (length > s->out_state_capabilities.bLength) {
                length = s->out_state_capabilities.bLength;
            }
            memcpy(data, &s->out_state_capabilities, length);
            p->actual_length = length;
        } else {
            p->status = USB_RET_STALL;
            assert(false);
        }
        break;
    case ((USB_DIR_IN|USB_TYPE_CLASS|USB_RECIP_DEVICE)<<8)
             | USB_REQ_GET_DESCRIPTOR:
        /* FIXME: ! */
        DPRINTF("xid unknown xpad request 0x%x: value = 0x%x\n",
                request, value);
        memset(data, 0x00, length);
        //FIXME: Intended for the hub: usbd_get_hub_descriptor, UT_READ_CLASS?!
        p->status = USB_RET_STALL;
        //assert(false);
        break;
    case ((USB_DIR_OUT|USB_TYPE_STANDARD|USB_RECIP_ENDPOINT)<<8)
             | USB_REQ_CLEAR_FEATURE:
        /* FIXME: ! */
        DPRINTF("xid unknown xpad request 0x%x: value = 0x%x\n",
                request, value);
        memset(data, 0x00, length);
        p->status = USB_RET_STALL;
        break;
    default:
        DPRINTF("xid USB stalled on request 0x%x value 0x%x\n", request, value);
        p->status = USB_RET_STALL;
        assert(false);
        break;
    }
}

static void usb_xid_handle_data(USBDevice *dev, USBPacket *p)
{
    USBXIDState *s = DO_UPCAST(USBXIDState, dev, dev);

    DPRINTF("xid handle_data 0x%x %d 0x%zx\n", p->pid, p->ep->nr, p->iov.size);

    switch (p->pid) {
    case USB_TOKEN_IN:
        if (p->ep->nr == 2) {
            update_input(s);
            usb_packet_copy(p, &s->in_state, s->in_state.bLength);
        } else {
            assert(false);
        }
        break;
    case USB_TOKEN_OUT:
        if (p->ep->nr == 1) {
            usb_packet_copy(p, &s->out_state, s->out_state.bLength);
            update_output(s);
        } else {
            assert(false);
        }
        break;
    default:
        p->status = USB_RET_STALL;
        assert(false);
        break;
    }
}

#if 0
static void usb_xid_handle_destroy(USBDevice *dev)
{
    USBXIDState *s = DO_UPCAST(USBXIDState, dev, dev);
    DPRINTF("xid handle_destroy\n");
}
#endif

static void usb_mj9k_gamepad_unrealize(USBDevice *dev)
{
}

static void usb_xid_class_initfn(ObjectClass *klass, void *data)
{
    USBDeviceClass *uc = USB_DEVICE_CLASS(klass);

    uc->handle_reset   = usb_xid_handle_reset;
    uc->handle_control = usb_xid_handle_control;
    uc->handle_data    = usb_xid_handle_data;
    // uc->handle_destroy = usb_xid_handle_destroy;
    uc->handle_attach  = usb_desc_attach;
}

static void usb_mj9k_gamepad_realize(USBDevice *dev, Error **errp)
{
    USBXIDState *s = USB_XID(dev);
    usb_desc_create_serial(dev);
    usb_desc_init(dev);
    s->intr = usb_ep_get(dev, USB_TOKEN_IN, 2);

    s->in_state.bLength = sizeof(s->in_state);
    s->in_state.bReportId = 0;

    s->out_state.bLength = sizeof(s->out_state);
    s->out_state.bReportId = 0;

    s->xid_desc = &desc_xid_mj9k_gamepad;

    memset(&s->in_state_capabilities, 0xFF, sizeof(s->in_state_capabilities));
    s->in_state_capabilities.bLength = sizeof(s->in_state_capabilities);
    s->in_state_capabilities.bReportId = 0;

    memset(&s->out_state_capabilities, 0xFF, sizeof(s->out_state_capabilities));
    s->out_state_capabilities.bLength = sizeof(s->out_state_capabilities);
    s->out_state_capabilities.bReportId = 0;
}

static Property xid_properties[] = {
    DEFINE_PROP_UINT8("index", USBXIDState, device_index, 0),
    DEFINE_PROP_END_OF_LIST(),
};

static const VMStateDescription vmstate_usb_mj9k = {
    .name = TYPE_USB_MJ9K,
    .version_id = 1,
    .minimum_version_id = 1,
    .fields = (VMStateField[]) {
        VMSTATE_USB_DEVICE(dev, USBXIDState),
        // FIXME
        VMSTATE_END_OF_LIST()
    },
};

static void usb_mj9k_gamepad_class_initfn(ObjectClass *klass, void *data)
{
    DeviceClass *dc = DEVICE_CLASS(klass);
    USBDeviceClass *uc = USB_DEVICE_CLASS(klass);

    uc->product_desc   = MJ9K_DESCRIPTION;
    uc->usb_desc       = &desc_mj9k_gamepad;
    uc->realize        = usb_mj9k_gamepad_realize;
    uc->unrealize      = usb_mj9k_gamepad_unrealize;
    usb_xid_class_initfn(klass, data);
    set_bit(DEVICE_CATEGORY_INPUT, dc->categories);
    dc->vmsd  = &vmstate_usb_mj9k;
    device_class_set_props(dc, xid_properties);
    dc->desc  = MJ9K_DESCRIPTION;
}

static const TypeInfo usb_mj9k_gamepad_info = {
    .name          = TYPE_USB_MJ9K,
    .parent        = TYPE_USB_DEVICE,
    .instance_size = sizeof(USBXIDState),
    .class_init    = usb_mj9k_gamepad_class_initfn,
};

static void usb_mj9k_register_types(void)
{
    type_register_static(&usb_mj9k_gamepad_info);
}

type_init(usb_mj9k_register_types)
