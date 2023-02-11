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

using orgb::ConnectStatus;
using orgb::DeviceListResult;
using orgb::RequestStatus;
using orgb::DeviceType;

orgb::Client client("XEMU OpenRGB Client");
const orgb::Device * keyboard;

int openrgb_connect(void)
{
    ConnectStatus status = client.connect( "127.0.0.1" );  // you can also use Windows computer name
    if (status != ConnectStatus::Success) {
        DPRINTF( "Failed to connect to OpenRGB server: %s (code: %d)\n",
            enumString( status ), int( client.getLastSystemError() ) );
        return -1;
    }
    
    DeviceListResult result = client.requestDeviceList();
    if (result.status != RequestStatus::Success) {
        DPRINTF( "Failed to get device list: %s (code: %d)\n",
            enumString( result.status ), int( client.getLastSystemError() ) );
        return -2;
    }
    
    keyboard = result.devices.find( DeviceType::Keyboard );
    if (!keyboard) {
        DPRINTF( "No RGB keyboards found" );
        return -3;
    }
    
    const orgb::Mode * directMode = keyboard->findMode( "Direct" );
    if (!directMode) {
        DPRINTF( "Keyboard does not support direct mode" );
        return -4;
    }
    client.changeMode( *keyboard, *directMode );
    
    return 0;
}

void openrgb_disconnect(void)
{
    client.disconnect();   
    keyboard = NULL;
}

int openrgb_setKeyboardColor(uint8_t r, uint8_t g, uint8_t b)
{
    if (!client.isConnected()) {
        DPRINTF( "Client is not connnected" );
        return -1;
    }

    if (!keyboard) {
        DPRINTF( "No keyboard found" );
        return -2;
    }
    
    client.setDeviceColor(*keyboard, orgb::Color(r, g, b));
    return 0;
}

// Color should be in ARGB format
int openrgb_setKeyColor(const char * name, uint8_t r, uint8_t g, uint8_t b)
{
    if (!client.isConnected()) {
        DPRINTF( "Client is not connnected" );
        return -1;
    }

    if (!keyboard) {
        DPRINTF( "No keyboard found" );
        return -2;
    }

    const orgb::LED * light = keyboard->findLED(name);
    if (!light) {
        DPRINTF( "LED %s does not exist on this keyboard", name );
        return -3;
    }
    
    client.setLEDColor(*light, orgb::Color(r, g, b));
    return 0;
}

int openrgb_setScancodeColor(SDL_Scancode scan, uint8_t r, uint8_t g, uint8_t b)
{
    return openrgb_setKeyColor(SDL_GetScancodeName(scan), r, g, b);
}
