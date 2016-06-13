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
