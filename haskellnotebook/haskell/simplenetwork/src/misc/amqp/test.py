"""
 Simple Queue Test
 Date: 1/12/2008
"""

from amqplib.client_0_8 import Connection, Message

if __name__ == '__main__':
	print "Running"
	connect_args = {}
	connect_args['userid'] = 'guest'
	connect_args['password'] = 'guest'
	connect_args['host'] = '127.0.0.1'
	
	conn = Connection(**connect_args)
	ch = conn.channel()

	ch.access_request('/data', active=True, write=True, read=True)
	my_routing_key = 'unittest.test_queue'
	msg = Message('unittest message',
				  content_type='text/plain',
				  application_headers={'foo': 7, 'bar': 'baz'})
	qname, _, _ = ch.queue_declare()
	ch.queue_bind(qname, 'amq.direct', routing_key=my_routing_key)
	ch.basic_publish(msg, 'amq.direct', routing_key=my_routing_key)
	msg2 = ch.basic_get(qname, no_ack=True)
	print msg2.application_headers
	conn.close()
	print "Done"
