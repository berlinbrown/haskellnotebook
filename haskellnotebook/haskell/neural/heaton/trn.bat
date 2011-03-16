@echo off
%1 %2 %3 %4
java -classpath "dist\lib\NeuralNetworkGame.jar" -Xms120m -Xmx200m org.berlin.neural.heaton.tictac.NeuralTicTacToe train NeuralBlank MinMax 
