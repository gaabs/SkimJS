var arr = [1,3,5-1,-1,0,4];
var n = arr.length;
var resposta = 0;
var soma = 0;

function max(a, b){
    if ( a < b ){
        return b;
    } else {
        return a;
    }
}

for(var i = 0; i < n; i++ ){
    soma = soma + arr[i];
    if ( soma < 0 ){
        soma = 0;
    }
    resposta = max(resposta, soma);
}
