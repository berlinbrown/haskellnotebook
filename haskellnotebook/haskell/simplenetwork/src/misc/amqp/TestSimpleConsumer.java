/* 
 * TestSimpleConsumer.java
 * Dec 30, 2007
 */
package org.spirit.misc_no_commit;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionParameters;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.QueueingConsumer;

/**
 * Also see:
 * http://www.rabbitmq.com/api-guide.html#connecting
 * http://www.rabbitmq.com/javadoc/
 * 
 * @author bbrown
 */
public class TestSimpleConsumer {

    public static void main(String[] args) {
        try {
            String hostName = (args.length > 0) ? args[0] : "localhost";
            int portNumber = (args.length > 1) ? Integer.parseInt(args[1]) : AMQP.PROTOCOL.PORT;

            ConnectionParameters params = new ConnectionParameters();
            params.setUsername("guest");
            params.setPassword("guest");
            ConnectionFactory connFactory = new ConnectionFactory(params);            
            Connection conn = connFactory.newConnection(hostName, portNumber);            

            final Channel ch = conn.createChannel();
            int ticket = ch.accessRequest("/data");
            ch.queueDeclare(ticket, "amq.direct");
            // channel.basicConsume(ticket, queueName, noAck, consumer
            QueueingConsumer consumer = new QueueingConsumer(ch);
            ch.basicConsume(ticket, "amq.direct", consumer);
            while (true) {
                QueueingConsumer.Delivery delivery = consumer.nextDelivery();
                System.out.println("Message: " + new String(delivery.getBody()));
                ch.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
            }
        } catch (Exception ex) {
            System.err.println("Main thread caught exception: " + ex);
            ex.printStackTrace();
            System.exit(1);
        }
    }
	
}	
