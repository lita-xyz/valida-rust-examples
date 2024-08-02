#![no_std]
#![feature(start)]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[start]
fn main(_argc: isize, _argv: *const *const u8) -> isize {

    extern { fn read_stdin() -> u8;}
    extern { fn write_stdout(n: u8);}
    fn transform_u32_to_array_of_u8(x:u32) -> [u8;4] {
        let b1 : u8 = ((x >> 24) & 0xff) as u8;
        let b2 : u8 = ((x >> 16) & 0xff) as u8;
        let b3 : u8 = ((x >> 8) & 0xff) as u8;
        let b4 : u8 = (x & 0xff) as u8;
        return [b4, b3, b2, b1]
    }
    unsafe {
    let n = read_stdin();
    let mut a: u32 = 0;
    let mut b: u32 = 1;
    let mut sum: u32;
    for _ in 1..n {
        sum = a + b;
        a = b;
        b = sum;
        }
    let b_as_bytes: [u8; 4] = transform_u32_to_array_of_u8(b);
    for i in 0..4 {
        write_stdout(b_as_bytes[i]);
        }
    }
    return 0;
}
