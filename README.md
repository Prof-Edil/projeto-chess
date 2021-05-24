# Projeto Chess

Para iniciar o jogo, basta baixar o repositório e executar os seguintes comandos:

```console
$ stack build
$ stack run
```

## Especificações do Projeto

O projeto consiste em um tabuleiro de xadrez, em que dois jogadores poderão disputar uma partida entre si.

Algumas ferramentas, como a detecção de xeques e xeque-mate não foram implementadas, porém todas as movimentações estão funcionando corretamente, e o jogo está funcionando perfeitamente.

Como não temos xeque-mate, o fim da partida ocorre com a captura do rei adversário.

[![Watch the video](https://i.imgur.com/vKb2F1B.png)](https://youtu.be/uK7VzqnYnEw)

## Regras do Xadrez:

- Jogadores - 2 jogadores
- Peças - 32 peças, sendo 16 brancas e 16 pretas. Ambas as cores possuem:
	- 2 torres
	- 2 cavalos
	- 2 bispos
	- 1 rainha
	- 1 rei
	- 8 peões
- Tabuleiro - Tabuleiro de 64 casas de cores alternadas, no formato 8x8, sendo as linhas enumeradas de 1-8, e as colunas de A-H. As casas claras são denominadas de "brancas" e as escuras de "pretas", sendo A1 uma casa preta.

- Arranjo inicial das peças - As peças são dispostas sobre o tabuleiro conforme a figura a seguir:

	![posição inical do tabuleiro](http://2.bp.blogspot.com/_YuN6ZIQ3nr8/TQ93EsyTc-I/AAAAAAAAABQ/1yVXvUFJPa0/s1600/posicao+inicial+das+pe%25C3%25A7as.jpg)

- Condução do jogo: 
	Por convenção, o jogador com as peças brancas faz a primeira jogada, e a partir de então as jogadas são alternadas. A partida continua até que um jogador aplique o xeque-mate, que é a ameaça de captura do rei adversário de modo que ele não consiga escapar ou impedir o ataque, ou um dos jogadores abandone a partida ou o empate seja declarado.

- Movimento e captura das peças:
	- Torre - A torre se movimenta pelas linhas (horizontais) e pelas colunas (verticais), não podendo se mover pelas diagonais. Ela se movimenta quantas casas quiser, porém em apenas um sentido em cada jogada.
	- Bispo - O bispo se movimenta nas diagonais, sem nunca alterar a cor da casa, em apenas um sentido em cada jogada. Cada jogador possui um bispo que se movimenta nas casas claras e um que se movimenta nas escuras.
	- Dama - A dama (ou rainha) é a peça mais poderosa, pois pode se movimentar na horizontal, vertical ou diagonal, quantas casas quiser ou puder, porém apenas em um sentido cada jogada.
	- Rei - O rei se movimenta em qualquer direção, apenas uma casa. Ele não pode se movimentar para uma casa que esteja ameaçada por uma peça adversária, nem capturar uma peça que esteja sendo defendida por outra peça adversária. O rei também não pode aplicar xeque-mate ao outro rei.
	- Cavalo - O cavalo se movimenta em forma de "L", sendo duas casas na vertical e uma na horizontal, ou duas na horizontal e uma na vertical. Ele pode pular as peças aliadas e adversárias, e captura quando a peça estiver na casa final do movimento.
	- Peão - O peão se movimenta apenas uma casa para a frente, na vertical. Quando o peão alcança a última fileira do tabuleiro, ele é promovido, podendo se tornar uma dama, uma torre, um cavalo ou um bispo, conforme o desejo do jogador. Na primeira movimentação do peão, ele pode andar uma ou duas casas para a frente, desde que as casas estejam livres. O movimento de captura do peão é diferente da forma que ele se movimenta, sendo de uma casa na diagonal a sua frente. Se um peão encontrar uma peça na vertical a sua frente, ele fica impedido de se mover até que a peça saia ou haja alguma peça adversária em sua diagonal a ser capturada.

- Movimentos extraordinários:
	- Roque - O roque consiste no rei se mover duas casas em direção a torre, e a torre se move para a casa do outro lado do rei, adjacente a ele. O movimento só é permitido nas seguintes condições:
		1. O rei e a torre não podem ter sido movidos desde o início da partida.
		2. Não existam peças entre o rei e a torre.
		3. O rei não pode estar em xeque, nem passar ou terminar em uma casa ameaçada.
		4. O rei e a torre devem estar na mesma fileira.
	- Tomada en passant - quando o peão se movimenta duas casas na sua primeira movimentação, e termina ao lado de um peão adversário, o peão adversário pode fazer a captura en passant, em que, ao capturar, ele ocupará a casa que o peão saltou ao mover duas casas. Essa captura só pode ser realizada imediatamente após o movimento do peão (saltar duas casas).
	- Promoção de peão - Ao peão alcançar a última fileira, ele é promovido a dama, cavalo, bispo ou torre da mesma cor, ficando a promoção a critério do jogador. A escolha não é limitada às peças previamente capturadas.

- Término da partida:
	A partida pode terminar em vitória de algum jogador ou em empate.

	- Vitória - A vitória pode ocorrer de duas formas, que são:
		1. O jogador adversário abandona a partida
		2. É aplicado um xeque-mate no rei adversário.

	- Empate - Uma partida é considerada empatada, quando: 
		1. Se o jogador não possuir nenhuma jogada legal, com o rei ou qualquer outra peça, é considerado empate por afogamento.
		2. Caso um jogador proponha o empate e o outro aceite, a partida é dada como empatada.
		3. Quando nenhum dos reis tenha material suficiente para dar xeque-mate no rei adversário. É considerado material insuficiente um rei, um rei e um bispo, um rei e um cavalo ou um rei e dois cavalos contra um rei sozinho.
		4. Ocorrer um xeque perpétuo, onde o jogador pode ficar permanentemente colocando o outro em xeque, não importando o que o outro faça, sem que se manifeste intenção de jogar diferente.
		5. São efetuados 50 lances, sem captura de peças ou movimentação de peão. A contagem é reiniciada ao se mover um peão ou capturar uma peça.
		6. Se uma dada posição ocorrer três vezes durante uma partida. Isto implica repetir a posição das peças, o lado a fazer o lance, e os lances possíveis. Se, por exemplo, na primeira vez que a posição se repetir um dos jogadores tiver direito de fazer roque e nas outras duas não tiver, não é a mesma posição. Da mesma forma, se o jogador ao qual cabe o lance tinha direito de fazer a captura en passant na primeira vez que a posição apareceu, então também não é a mesma posição. 	
