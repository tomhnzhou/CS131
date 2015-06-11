SERVER_PORT = {'Alford': 12712, 'Bolden': 12713, 
'Hamilton': 12714,'Parker': 12715,'Powell': 12716}

SERVER_FRIENDS = {
	'Alford': ['Parker', 'Powell'],
	'Bolden': ['Parker', 'Powell'],
	'Hamilton': ['Parker'],
	'Parker': ['Alford', 'Bolden', 'Hamilton'],
	'Powell': ['Alford', 'Bolden']
}

GOOGLE_KEY = 'AIzaSyBL4_4I69cdin4CYWZ8BUATaIIQ7IdekCw'
GOOGLE_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'

def find_mst(root):
	path, n = find_mst_rec(root, [])
	return path

def find_mst_rec(root, marked_nodes):
	#print "Root: ", root
	marked_nodes.append(root)
	path = {}
	next_node = []
	for server in SERVER_FRIENDS[root]:
		if server in marked_nodes: 
			continue
		next_node.append(server)
		#print "Root: ", root, ", recursively going to ", server
		e, v = find_mst_rec(server, marked_nodes)
		marked_nodes.append(v)
		if e:
			path.update(e)
	#print "Root: ", root, ", next_node: ", next_node
	#print "Root: ", root, ", path: ", path
	path[root] = next_node
	return [path, marked_nodes]


