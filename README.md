# Zig-GigatronEmulator
A Gigatron emulator written in Zig

This is a casual project I work on for my own amusement. I'd never written an emulator before, and CHIP-8 seemed too trivial, so I choose the Gigatron instead.

## Status
 - Emulation of the Gigatron itself is more-or-less cycle accurate, with the exception of how undefined behavior behaves relative to real-world.
 - VGA is emulated well enough to show the Gigatron ROM's intended image, although it could be improved in relation to real VGA hardware.
 - Blinkenlights blink.
 - Audio is emulated, although I am skeptical that the implementation of the bandpass filtering is correct.
 - Gamepad emulated (though no platforms yet support an actual gamepad).
 - Pluggy McPlugface key inputs work, including gamepad emulation. tinyBasic save/load works (F3 loads, not ctrl-F3).
 
## Platforms
 - Currently only Windows Vista or higher is supported on x64.
 
## Building
 - Clone the Git repository
 - Run `zig build`

## Dependencies
 - This project depends on zigwin32 (https://github.com/marlersoft/zigwin32) for the Windows build (included)
 
## Running
 - Place resulting binary in a directory with a Gigatron rom file named "gigatron.rom". ROMv5a can be obtained here: https://github.com/kervinck/gigatron-rom/blob/master/ROMv5a.rom
 
 ## Images
 ![ROMv5a Menu](https://github.com/tgschultz/Zig-GigatronEmulator/blob/main/snaps/rom5a-menu.png)
 ![Snake](https://github.com/tgschultz/Zig-GigatronEmulator/blob/main/snaps/snake.png)
 ![Racer](https://github.com/tgschultz/Zig-GigatronEmulator/blob/main/snaps/racer.png)
 ![Tetronis](https://github.com/tgschultz/Zig-GigatronEmulator/blob/main/snaps/tetronis.png)
