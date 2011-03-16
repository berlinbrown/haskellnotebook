@echo off

java -classpath "dist\lib\NeuralNetworkGame.jar" -Xms100m -Xmx200m org.berlin.neural.heaton.tictac.NeuralTicTacToe match NeuralLoad Random 
