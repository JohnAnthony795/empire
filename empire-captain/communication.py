import tools

debug = tools.debug

class Communication:

	def __init__(self, parser, server, server_fd):
		self.parser = parser
		self.server_fd = server_fd
		self.server = server
		self.player_turn = False

	def info(self, message):
		debug("INFO: %s" % message)
		assert self.player_turn
		self.server.send(message + "\n")
		response = self.server_fd.readline().strip()
		debug("RESPONSE.1: %s" % response)
		message_get_action = self.server_fd.readline().strip()
		assert message_get_action == "get_action"
		return response
		
	def action(self, message):
		debug("ACTION: %s" % message)
		assert self.player_turn
		assert message != "end_turn"
		self.server.send(message + "\n")
		response = self.server_fd.readline().strip()
		debug("RESPONSE.1: %s" % response)
		while response != "get_action":
			self.parser.parse(response)
			response = self.server_fd.readline().strip()
			debug("RESPONSE.2: %s" % response)

	def end_turn(self):
		debug("END_TURN")
		assert self.player_turn
		self.server.send("end_turn\n")
		self.player_turn = False

	def wait(self):
		assert not self.player_turn
		response = self.server_fd.readline().strip()
		debug("RESPONSE.1: %s" % response)
		while response != "get_action":
			self.parser.parse(response)
			response = self.server_fd.readline().strip()
			debug("RESPONSE.2: %s" % response)
		self.player_turn = True
