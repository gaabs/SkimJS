function max(a, b){
    if ( a < b ){
        return b;
    } else {
        return a;
    }
}

function kadane(x){
    var n = x.length;
    var resposta = 0;
    var soma = 0;
    for(var i = 0; i < n; i++ ){
        soma = soma + x[i];
        if ( soma < 0 ){
            soma = 0;
        }
        resposta = max(resposta, soma);
    }
    return resposta;
}

var arr = [1,3,5-1,-1,0,4];

var a = kadane(arr);
