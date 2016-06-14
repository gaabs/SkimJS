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

function test2(){
	c = 0;
	var c = -1;
}

function alterGlobal(){
	d=d+5;
	var k = 0;
	return d;
}

var a = id(10);
var b = fat1(10);
var c = fat2(10);
var d = test();
test2();
var e = alterGlobal();
d;
verdade = [1,2,3] == [1,2,3]
mentira = [1,2,3] == [3,4,5]
verdade2 = lineCond(1,1);
mentira2 = lineCond(1,0);