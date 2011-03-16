/* 
 * Test.java
 * Dec 30, 2007
 */
package org.spirit.misc_no_commit;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;

/**
 * @author bbrown
 */
public class TestProducer {
	
	 public static void main(String[] args) {
	        try {
	            String hostName = (args.length > 0) ? args[0] : "localhost";
	            int portNumber = (args.length > 1) ? Integer.parseInt(args[1]) : AMQP.PROTOCOL.PORT;
	            String message = (args.length > 2) ? args[2] :
	                "the time is " + new java.util.Date().toString();

	            Connection conn = new ConnectionFactory().newConnection(hostName, portNumber);
	            Channel ch = conn.createChannel();
	            int ticket = ch.accessRequest("/data");
	            	
	            // Where exchange name is = ""
	            ch.queueDeclare(ticket, "amq.direct");	            	            
	            ch.queueBind(ticket, "amq.direct", "", "unittest.test_queue");
	            
	            // [ channel.basicPublish(ticket, exchangeName, routingKey, null, messageBodyBytes); ]
	            ch.basicPublish(ticket, "", "unittest.test_queue", null, message.getBytes());
	            ch.close(200, "Closing the channel");
	            conn.close(200, "Closing the connection");
	            System.out.println("Producer exiting...");
	        } catch (Exception e) {
	            System.err.println("Main thread caught exception: " + e);
	            e.printStackTrace();
	            System.exit(1);
	        }
	    }
	
}
