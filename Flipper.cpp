int Flipper_VN(int **MatViz, int lin, int col, int **Teste, int W){

int Pop=lin*col;

int vallin=0;
int valcol=0;
while(vallin<lin && valcol<col){
	if(Teste[vallin][valcol]==W)
		break;
	vallin++;
	valcol++;
	if(vallin<lin && valcol>=col)
		valcol=0;
}


if((vallin-1)>=0){
	MatViz[(vallin-1)][valcol]+=;
}

if((valcol-1)>=0){
	MatViz[vallin][(valcol-1)]+=;
}

if((vallin+1)<=(lin-1)){
	MatViz[(vallin+1)][valcol]+=;
}

if((valcol+1)<=(col-1)){
	MatViz[vallin][(valcol+1)]+=;
}

return MatViz;

}