#![no_main]
#![no_std]
#![feature(start)]
#![allow(arithmetic_overflow)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let x = 10;
    let z = x > 9;
    if z {
        loop {}
    }
    else {
        return 0;
    }
}

