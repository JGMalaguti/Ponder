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
	if (vallin < lin && valcol < col)
	{
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
	}

	return MatViz;
}
