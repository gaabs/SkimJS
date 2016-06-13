function factorial(x){
    if ( x == 1 ){
        return x;
    } else {
        return factorial(x-1) * x;
    }
}

fact = factorial(10);
