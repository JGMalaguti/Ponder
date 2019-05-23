double Amostragem_Ponder_Simples_VN(double **Matriz, int lin, int col, int n, double K){

int Pop=lin*col;

int **MatPop =new int*[lin];
int **MatViz =new int*[lin];
int **Teste =new int*[lin];

for(int i=0;i<lin;i++){
	MatPop=new int[col];
	MatViz=new int[col];
	Teste=new int[col];
}

int val=1;

for(int i=0;i<lin;i++){
	for(int j=0;j<col;j++){
		MatPop[i][j]=1;
		MatViz[i][j]=0;
		Teste[i][j]=val;
		val++;
	}
}

srand(time(null));
int Am = rand() % Pop+1;
int vallin=0;
int valcol=0;
while(vallin<lin && valcol<col){
	if(Teste[vallin][valcol]==Am)
		break;
	vallin++;
	valcol++;
	if(vallin<lin && valcol>=col)
		valcol=0;
}

























}