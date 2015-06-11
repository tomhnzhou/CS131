from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log
from twisted.web.client import getPage
from twisted.application import service, internet

import string, logging, sys, time, urllib, json

from proxyinfo import *

class ProxyServerProtocol(LineReceiver):
	def __init__(self, factory):
		self.factory = factory
		print self.factory.name, "> Server Protocol initiated"
	

	def lineReceived(self, line):
		print self.factory.name, "> Line Received:", line
		logging.info('{0}> Line Received: {1}'.format(self.factory.name, line))

		words = line.split()
		if not words:
			self.invalidCommandReceived(line)
			return

		if (words[0] == "IAMAT"):
			self.IAMATreceived(line)
		elif (words[0] == "AT"):
			self.ATreceived(line)
		elif (words[0] == "WHATSAT"):
			self.WHATSATreceived(line)
		else:
			self.invalidCommandReceived(line)

		self.transport.loseConnection()

	def IAMATreceived(self, line):
		words = line.split()
		clientId = words[1]
		clientPos = words[2]
		clientTime = words[3]

		server_time = time.time()
		time_diff = server_time - float(clientTime)

		if(time_diff < 0):
			time_diff_str = '-{0}'.format(time_diff)
		else:
			time_diff_str = '+{0}'.format(time_diff)

		response_msg = 'AT {0} {1} {2} {3} {4}'.format(self.factory.name, time_diff, clientId, clientPos, server_time)
		self.factory.loc_dict[clientId] = response_msg

		print self.factory.name, "> Response to client: ", response_msg
		self.sendLine(response_msg)
		logging.info('{0}> Sent response to client: {1}'.format(self.factory.name, clientId))

		mst = find_mst(self.factory.name)

		for server in mst[self.factory.name]:
			print self.factory.name, "> Sending location update to: ", server
			reactor.connectTCP("localhost", SERVER_PORT[server], ProxyClientFactory(response_msg))
			logging.info('{0}> Location update sent to {1}'.format(self.factory.name, server))

	def ATreceived(self, line):
		words = line.split()
		senderName = words[1]
		clientId = words[3]

		self.factory.loc_dict[clientId] = line
		mst = find_mst(senderName)

		for server in mst[self.factory.name]:
			print self.factory.name, "> Sending location update to: ", server
			reactor.connectTCP("localhost", SERVER_PORT[server], ProxyClientFactory(line))
			logging.info('{0}> Location update sent to {1}'.format(self.factory.name, server))

	def WHATSATreceived(self, line):
		words = line.split()
		clientId = words[1]
		radius = float(words[2]) * 1000
		info_limit = int(words[3])

		if not self.factory.loc_dict.has_key(clientId):
			print self.factory.name, "> Client location not found"
			logging.info('{0}> Location of Client \'{1}\' is not found'.format(self.factory.name, clientId))
			return

		query = self.factory.loc_dict[clientId]
		print "Query is: ", query
		query_words = query.split()
		clientPos = query_words[4]

		pos1 = clientPos[1:(len(clientPos)-1)].find('+')
		pos2 = clientPos[1:(len(clientPos)-1)].find('-')
		pos =  pos1+1 if pos1 > 0 else pos2+1
		location = clientPos[0:pos-1] + ',' + clientPos[pos:(len(clientPos)-1)]

		url = '{0}location={1}&radius={2}&key={3}'.format(GOOGLE_URL, location, radius, GOOGLE_KEY)
		google_response = urllib.urlopen(url)
		data = json.loads(google_response.read())
		n_results = len(data['results'])
		if(n_results > info_limit):
			new_result = data['results'][0:info_limit-1]
			data['results'] = new_result
		formatted_response = json.dumps(data, indent=4)

		response_msg = '{0}\n{1}\n\n'.format(query, formatted_response)
		print self.factory.name, "> Sending response to client..."
		self.sendLine(response_msg)
		logging.info('{0}> Response sent to client:\n{1}'.format(self.factory.name, formatted_response))

	def invalidCommandReceived(self, line):
		err_msg = '? {0}'.format(line)
		self.sendLine(err_msg)
		
		
	def connectionMade(self):
		print self.factory.name, "> Connection Made"

		

class ProxyServerFactory(protocol.Factory):
	def __init__(self, name):
		self.name = name
		self.port_no = SERVER_PORT[name]
		self.loc_dict = {}
		log_filename = name + ".log"
		logging.basicConfig(filename=log_filename, level=logging.DEBUG)
		logging.info('{0}> Server started'.format(name))

	def buildProtocol(self, addr):
		return ProxyServerProtocol(self)

class ProxyClientProtocol(LineReceiver):
	def __init__(self, factory):
		print "Client Protocol Initiated"
		self.factory = factory

	def lineReceived(self, line):
		print "Response from Server: ", line
		self.transport.loseConnection()

	def connectionMade(self):
		print "Connected to server"
		self.sendLine(self.factory.query)

class ProxyClientFactory(protocol.ClientFactory):
	def __init__(self, query):
		self.query = query

	def buildProtocol(self, addr):
		return ProxyClientProtocol(self)

	def clientConnectionFailed(self, connector, reason):
		print "Connection failed."

	def clientConnectionLost(self, connector, reason):
		print "Connection lost."

server_name = sys.argv[1]
portno = SERVER_PORT[server_name]

print "Running server:", server_name

reactor.listenTCP(portno, ProxyServerFactory(server_name))
reactor.run()