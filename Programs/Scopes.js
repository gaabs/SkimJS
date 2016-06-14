function f(){
    c = 5;
}

function f2(){
    c = c - 1;
}

f();

for(var i = 0; i < 10; i++ ){
    if ( i == 5 ){
        break;
    }
    f2();
}

var x = 0;

function h(){
	x = 10;
}

function g(){
	var x = 5;
	h();
}

g();
x;