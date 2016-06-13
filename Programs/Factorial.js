function factorialLeft(x){
    if ( x == 1 ){
        return x;
    } else {
        return factorialLeft(x-1) * x;
    }
}

function factorialRight(x){
    if ( x == 1 ){
        return x;
    } else {
        return x * factorialRight(x-1);
    }
}

factLeft = factorialLeft(10);
factRight = factorialRight(10);
