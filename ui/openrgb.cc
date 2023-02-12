/*
 * QEMU OpenRGB Keyboard Light Controller
 *
 * Copyright (c) 2023 Samuel Deutsch
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "OpenRGB/Client.hpp"
#include <SDL.h>
#include "openrgb.h"

#define DEBUG_OPENRGB

#ifdef DEBUG_OPENRGB
#define DPRINTF(fmt, ...) \
    do { fprintf(stderr, fmt, ## __VA_ARGS__); } while (0)
#else
#define DPRINTF(fmt, ...) \
    do { } while (0)
#endif

#define OPENRGB_UPDATE_INTERVAL 200

using orgb::ConnectStatus;
using orgb::DeviceListResult;
using orgb::RequestStatus;
using orgb::UpdateStatus;
using orgb::DeviceType;
using std::string;
using std::vector;

typedef struct openrgb_ScancodeColor {
    SDL_Scancode scan;
    uint8_t r, g, b;
} openrgb_ScancodeColor;

orgb::Client client("XEMU OpenRGB Client");
orgb::DeviceListResult devList;
vector<openrgb_ScancodeColor> openrgb_updates;

int openrgb_updateDeviceList(void)
{
    UpdateStatus status = client.checkForDeviceUpdates();
    if (status == UpdateStatus::UpToDate) return 0;
    
    if (status == UpdateStatus::OutOfDate) {
        devList = client.requestDeviceList();
        if (devList.status != RequestStatus::Success) {
            DPRINTF( "Failed to get device list: %s (code: %d)\n",
                enumString( devList.status ), int( client.getLastSystemError() ) );
            return -2;
        }
        return 1;
    }
    
    return -1;
}

static uint32_t nextUpdate;

int openrgb_tick(void)
{
    uint32_t now = SDL_GetTicks();
    if (now < nextUpdate) return 0;
    nextUpdate = now + OPENRGB_UPDATE_INTERVAL;
    return 1;
}

int openrgb_connect(void)
{
    ConnectStatus status = client.connect("192.168.1.81");//"127.0.0.1");
    if (status != ConnectStatus::Success) {
        DPRINTF( "Failed to connect to OpenRGB server: %s (code: %d)\n",
            enumString( status ), int( client.getLastSystemError() ) );
        return -1;
    }
    
    devList = client.requestDeviceList();
    if (devList.status != RequestStatus::Success) {
        DPRINTF( "Failed to get device list: %s (code: %d)\n",
            enumString( devList.status ), int( client.getLastSystemError() ) );
        return -2;
    }
    
    const orgb::Device * keyboard = devList.devices.find(DeviceType::Keyboard);
    if (!keyboard) {
        DPRINTF( "No RGB keyboards found\n" );
        return -3;
    }
    
#ifdef DEBUG_OPENRGB
    print(*keyboard);
#endif
    
    const orgb::Mode * directMode = keyboard->findMode( "Direct" );
    if (!directMode) {
        DPRINTF( "Keyboard does not support direct mode\n" );
        return -4;
    }
    client.changeMode( *keyboard, *directMode );
    
    return 0;
}

void openrgb_disconnect(void)
{
    client.disconnect();
}

int openrgb_setKeyboardColor(uint8_t r, uint8_t g, uint8_t b)
{
    if (openrgb_updateDeviceList() < 0) {
        DPRINTF( "Failed to get device list\n" );
        return -1;
    }
    
    const orgb::Device * keyboard = devList.devices.find(DeviceType::Keyboard);
    if (!keyboard) {
        DPRINTF( "No RGB keyboards found\n" );
        return -2;
    }
    
    DPRINTF("MASTER KB RESET\n");
    
    client.setDeviceColor(*keyboard, orgb::Color(r, g, b));
    return 0;
}

// Color should be in ARGB format
int openrgb_commitColors(bool forceUpdate)
{
    if (openrgb_updateDeviceList() < 0) {
        DPRINTF( "Failed to get device list\n" );
        return -1;
    }
    
    const orgb::Device * keyboard = devList.devices.find(DeviceType::Keyboard);
    if (!keyboard) {
        DPRINTF( "No RGB keyboards found\n" );
        return -2;
    }
    
    for (auto &update : openrgb_updates) {
        string name = "Key: " + string(SDL_GetScancodeName(update.scan));
        if ( name == "Key: PageUp"   ) name = "Key: Page Up";
        if ( name == "Key: PageDown" ) name = "Key: Page Down";
        
//        DPRINTF("Setting light '%s' to %d %d %d\n", name.c_str(), update.r, update.g, update.b);
        
        const orgb::LED * light = keyboard->findLED(name);
        if (light) {
            client.setLEDColor(*light, orgb::Color(update.r, update.g, update.b));
        } else {
            DPRINTF( "LED '%s' does not exist on this keyboard\n", name.c_str() );
        }
    }
    
    if (forceUpdate) {
        client.requestDeviceInfo(keyboard->idx);
    }
    
    // Empty update list and return number of keys updated
    int count = openrgb_updates.size();
    openrgb_updates.clear();
    return count;
}

int openrgb_setScancodeColor(SDL_Scancode scan, uint8_t r, uint8_t g, uint8_t b)
{
    if (!client.isConnected()) {
        DPRINTF( "Client is not connnected\n" );
        return -1;
    }

    openrgb_updates.push_back((openrgb_ScancodeColor){
        .scan = scan,
        .r = r,
        .g = g,
        .b = b
    });
    
    return 0;
}
