function id(x){
	return x;
}

function fat1(x){
	if (x == 1) {
		return 1;
	}
	return fat1(x-1)*x;
}

function fat2(x){
	if (x == 1){
		return 1;
	}
	return x*fat2(x-1);
}

function lineCond(x,y){
	if (x == y) return true;
	
	while (x != y) return false;
}

function test(){
	var c = -1;
	return 0;
}

var a = id(10);
var b = fat1(10);
var c = fat2(10);
var d = test();
d;
[1,2,3] == [3,4,5]
//lineCond(1,0);