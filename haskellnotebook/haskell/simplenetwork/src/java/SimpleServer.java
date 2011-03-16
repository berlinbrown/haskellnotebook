import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.net.ServerSocket;
import java.util.Date;


/**
 * SimpleServer.java
 */
public class SimpleServer {

	public static final int DEFAULT_PORT = 5672;

	private int port = DEFAULT_PORT;
	private ServerSocket server;

	public SimpleServer(int port) {
		this.port = port;
	}
	
	public void runServer() {
		try {
			server = new ServerSocket(this.port);			
			System.out.println("server bound to port=" + this.port);
		} catch (IOException e) {
			System.out.println("Could not listen on port=" + this.port);
			System.exit(-1);
		}	
		try {		
			while (true) {
				SimpleServerThread clientThread;
				try {
					// server.accept returns a client connection
					System.out.println("waiting for client connection...");								
					Socket clientSocket = server.accept();
					if  (clientSocket == null) {
						System.out.println("ERROR: invalid socket connection, closing.");
						return;					
					} else {
						System.out.println("connection made=" + clientSocket);
					}
					clientThread = new SimpleServerThread(clientSocket);
					Thread t = new Thread(clientThread);
					t.start();
				} catch (IOException e) {
					System.out.println("Accept failed: " + this.port);
					System.exit(-1);
				}
			} // End of While
		} finally {
			try {
				System.out.println("Closing server connection");
				server.close();
			} catch (IOException e1) { }	
		}
	}
	public static void main(String[] args) {
		System.out.println("-- Running Server");
		SimpleServer server = new SimpleServer(DEFAULT_PORT);
		server.runServer();
		System.out.println("-- Done");

	}
}