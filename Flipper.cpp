int** Flipper_VN(int **MatViz, int lin, int col, int **Teste, int W)
{

	int Pop = lin * col;

	int vallin = 0;
	int valcol = 0;
	while (vallin < lin && valcol < col)
	{
		if (Teste[vallin][valcol] == W)
			break;
		vallin++;
		valcol++;
		if (vallin < lin && valcol >= col)
			valcol = 0;
	}

		//verifica se a linha de cima está no intervalo [0,lin)
		if ((vallin - 1) >= 0)
		{

			MatViz[(vallin - 1)][valcol] ++;
		}
		//verifica se a coluna à esquerda está no intervalo [0,col)
		if ((valcol - 1) >= 0)
		{
			MatViz[vallin][(valcol - 1)] ++;
		}
		//verifica se a linha de baixo está no intervalo [0,lin)
		if ((vallin + 1) < lin)
		{
			MatViz[(vallin + 1)][valcol] ++;
		}
		//verifica se a coluna à direita está no intervalo [0,col)
		if ((valcol + 1) < col)
		{
			MatViz[vallin][(valcol + 1)] ++;
		}


	return MatViz;
}

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

int** Flipper_EVN(int **MatViz, int lin, int col, int **Teste, int W)
{

	int Pop = lin * col;

	int vallin = 0;
	int valcol = 0;
	while (vallin < lin && valcol < col)
	{
		if (Teste[vallin][valcol] == W)
			break;
		vallin++;
		valcol++;
		if (vallin < lin && valcol >= col)
			valcol = 0;
	}

		//verifica se a linha de cima está no intervalo [0,lin)
		if ((vallin - 1) >= 0)
		{

			MatViz[(vallin - 1)][valcol] ++;
			
			if((vallin - 2) >= 0){
				MatViz[(vallin - 2)][valcol] ++;
			}
			
		}
		//verifica se a coluna à esquerda está no intervalo [0,col)
		if ((valcol - 1) >= 0)
		{
			MatViz[vallin][(valcol - 1)] ++;
			
			if((valcol - 2) >= 0){
				MatViz[vallin][(valcol - 2)] ++;
			}
		}
		//verifica se a linha de baixo está no intervalo [0,lin)
		if ((vallin + 1) < lin)
		{
			MatViz[(vallin + 1)][valcol] ++;
			
			if((vallin + 2) < lin){
				MatViz[(vallin + 2)][valcol] ++;
			}
		}
		//verifica se a coluna à direita está no intervalo [0,col)
		if ((valcol + 1) < col)
		{
			MatViz[vallin][(valcol + 1)] ++;
			
			if((valcol + 2) < col){
				MatViz[vallin][(valcol + 2)] ++;
			}
		}

	return MatViz;
}

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

int** Flipper_Moore(int **MatViz, int lin, int col, int **Teste, int W)
{

	int Pop = lin * col;

	int vallin = 0;
	int valcol = 0;
	while (vallin < lin && valcol < col)
	{
		if (Teste[vallin][valcol] == W)
			break;
		vallin++;
		valcol++;
		if (vallin < lin && valcol >= col)
			valcol = 0;
	}

		//verifica se a linha de cima está no intervalo [0,lin)
		if ((vallin - 1) >= 0)
		{

			MatViz[(vallin - 1)][valcol] ++;
			
			if ((valcol + 1) < col){
				MatViz[(vallin - 1)][valcol + 1] ++;
			}
		
			if ((valcol - 1) >= 0){
				MatViz[(vallin - 1)][valcol - 1] ++;
			}
			
		}
		//verifica se a linha de baixo está no intervalo [0,lin)
		if ((vallin + 1) < lin)
		{
			MatViz[(vallin + 1)][valcol] ++;
			
			if ((valcol + 1) < col){
				MatViz[(vallin + 1)][valcol + 1] ++;
			}
		
		}
		
		//verifica se a coluna à esquerda está no intervalo [0,col)
		if ((valcol - 1) >= 0)
		{
			MatViz[vallin][(valcol - 1)] ++;
			
			if((valcol - 2) >= 0){
				MatViz[vallin][(valcol - 2)] ++;
			}
			
		}
		
		//verifica se a coluna à direita está no intervalo [0,col)
		if ((valcol + 1) < col)
		{
			MatViz[vallin][(valcol + 1)] ++;
		}


	return MatViz;
}

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

int** Flipper_Manhattan(int **MatViz, int lin, int col, int **Teste, int W)
{

	int Pop = lin * col;

	int vallin = 0;
	int valcol = 0;
	while (vallin < lin && valcol < col)
	{
		if (Teste[vallin][valcol] == W)
			break;
		vallin++;
		valcol++;
		if (vallin < lin && valcol >= col)
			valcol = 0;
	}

		//verifica se a linha de cima está no intervalo [0,lin)
		if ((vallin - 1) >= 0)
		{

			MatViz[(vallin - 1)][valcol] ++;
			
			if ((valcol + 1) < col){
				MatViz[(vallin - 1)][valcol + 1] ++;
			}
		
			if ((valcol - 1) >= 0){
				MatViz[(vallin - 1)][valcol - 1] ++;
			}
						
			if((vallin - 2) >= 0){
				MatViz[(vallin - 2)][valcol] ++;
			}
			
		}
		//verifica se a linha de baixo está no intervalo [0,lin)
		if ((vallin + 1) < lin)
		{
			MatViz[(vallin + 1)][valcol] ++;
			
			if ((valcol + 1) < col){
				MatViz[(vallin + 1)][valcol + 1] ++;
			}
		
			if ((valcol - 1) >= 0){
				MatViz[(vallin + 1)][valcol - 1] ++;
			}
			
			if((vallin + 2) < lin){
				MatViz[(vallin + 2)][valcol] ++;
			}

		}
		
		//verifica se a coluna à esquerda está no intervalo [0,col)
		if ((valcol - 1) >= 0)
		{
			MatViz[vallin][(valcol - 1)] ++;
			
			if((valcol - 2) >= 0){
				MatViz[vallin][(valcol - 2)] ++;
			}
		}
		
		//verifica se a coluna à direita está no intervalo [0,col)
		if ((valcol + 1) < col)
		{
			MatViz[vallin][(valcol + 1)] ++;
			
			if((valcol + 2) < col){
				MatViz[vallin][(valcol + 2)] ++;
			}
		}


	return MatViz;
}
