var x = 5, y = 10;
var z = 0;
if (z) {
	z = x - y;
} else{
	var w = 33;
}
z = 0;
while (x){
	x = x - 1;
	z = z + 1;
}

function f(){
	x = 2;
}
f();
z;