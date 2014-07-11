use std::ptr::RawPtr;

fn main(){
	let foo:*const uint = RawPtr::null();
	let temp = unsafe{&*foo};
	println!("yes");
	println!("{}", temp);
}