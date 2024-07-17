#![no_std]
#![feature(start)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let x = 10;
    let z = x < 90;
    if z {
        loop {}
    }
    else {
        return 0;
    }
}

