#import behavior_tree
import decision_tree
import handler

class Parameters(handler.Handler):

	def __init__(self, the_situation):
		self.the_situation = the_situation
		self.initialized = False

	def update(self):
		if not self.initialized:
			self.initialized = True
			self.influence_city_intensity=10
			self.influence_city_step_length=1
			self.influence_city_nb_steps=5
			self.influence_unexplored_intensity=1
			self.influence_unexplored_step_length=1
			self.influence_unexplored_nb_steps=60
			self.influence_piece_nb_steps=40
			self.influence_piece_intensity_factor=40
			self.influence_piece_step_length_factor=10
			node = decision_tree.Node
			leaf = decision_tree.Leaf
			self.production_decision = \
				node("pp", "<", "ep", 8,
					leaf([(0, 0.8), (1, 0.2)]),
					node("pp", "<", "pc", 2,
						leaf([(0, 1)]),
						node("nCw", "=", "g", 0,	# If it is not a port.
							leaf([(0, 0.6), (1, 0.4)]),
							node("Cwpp", "<", "Cwep", 8,
								leaf([(3, 0.6), (4, 0.4)]),
								node("Cwpp", "<", "Cwp@2", 2,
									leaf([(3, 0.6), (4, 0.4)]),
									node("Cwp@2.full", ">", "Cwp@2", 0.8,
										leaf([(2, 1)]),
										node("Cwp@2.full", "<", "Cwp@2", 0.2,
											leaf([(0, 1)]),
											node("Cwp@2", "<", "CwNg", 1,	# The number of transport must be greater than then number of neighbors of the water.
												leaf([(2, 1)]),
												leaf([(0, 0.2), (1, 0.2), (3, 0.2), (4, 0.2)])
				))))))))
			self.behaviors = \
				{	"scout":
						("sequence",
							[	("sequence_memorize",
									[	("scout_choose_destination", )
									,	("find_path", )
									,	("selector",
											[	("while_success",
													("stepto", )
												)
											,	("selector",
													[	("?arrived", )
													,	("sequence",
															[	("inverser",
																	("?steps_left", )
																)
															,	("running", )
															]
														)
													,	("sequence",
															[	("find_path", )
															,	("running", )
															]
														)
														# No solution? Return success to call scout_choose_destination again.
													,	("success", )
													]
												)
											]
										)
									]
								)
							]
						)
				,	"move_body":
						("selector",
							[	("sequence",
									[	("?can_invade", )
									,	("select_nearest_enemy_city", )
									,	("link", "moveto")
									]
								)
							,	("sequence",
									[	("select_nearest_enemy_piece", )
									,	("link", "moveto")
									]
								)
							,	("sequence",
									[	("?can_invade", )
									,	("select_free_city", )
									,	("link", "moveto")
									]
								)
							,	("sequence",
									[	("?continent_contains_enemy", )
									,	("move_toward_enemy", )
									,	("link", "moveto", )
									]
								)
							,	("sequence",
									[	("?continent_contains_free_cities", )
									,	("move_toward_free_cities", )
									,	("link", "moveto", )
									]
								)
							,	("link", "scout")
							#,	("link", "random_walk")
							,	("sequence",
									[	("select_nearest_player_city", )
									,	("link", "moveto")
									]
								)
							]
						)
				,	"moveto_continent":
						("sequence_memorize",
							[	("find_path_to_group", )
							,	("selector",
									[	("while_success",
											("stepto", )
										)
									, 	("?arrived_to_group", )
									,	("sequence",
											[	("inverser",
													("?steps_left", )
												)
											,	("running", )
											]
										)
									]
								)
							]
						)
				,	"moveto":
						("sequence_memorize",
							[	("find_path", )
							,	("selector",
									[	("while_success",
											("stepto", )
										)
									, 	("?arrived", )
									,	("sequence",
											[	("inverser",
													("?steps_left", )
												)
											,	("running", )
											]
										)
									]
								)
							]
						)
				,	"moveto_transport":
						("sequence_memorize",
							[	("find_path", )
							,	("selector",
									[	("while_success",
											("sequence",
												[	("stepto", )
												,	("inverser",
														("?arrived", )
													)
												]
											)
										)
									, 	("sequence",
											[	("?arrived", )
											,	("reach_transport", )
											]
										)
									,	("sequence",
											[	("inverser",
													("?is_path_valid", )
												)
											,	("reset", )
											]
										)
									,	("running", )
									]
								)
							]
						)
				,	"wait":
						("success", )
				,	"random_walk":
						("while_running",
							("sequence",
								[	("?steps_left", )
								,	("random_walk", )
								,	("link", "moveto", )
								]
							)
						)
				,	"move_transport":
						("while_success",
							("sequence_memorize",
								[	("selector",
										[	("?in_city",
												("link", "random_walk" )
											)
										,	("success", )
										]
									)
								,	("sequence_memorize",
										[	("select_reachable_safe_continent_with_lot_of_player", )
										,	("link", "moveto_continent", )
										,	("take", )
										]
									)
								,	("sequence_memorize",
										[
											("selector",
												[	("select_reachable_continent_with_player_and_enemy", )
												,	("select_reachable_continent_with_nobody", )
												,	("select_reachable_continent_with_enemy", )
												,	("running", )
												]
											)
										,	("link", "moveto_continent", )
										,	("land", )
										]
									)
								]
							)
						)
				,	"move@0": ("link", "move_body")
				,	"move@1": ("link", "move_body")
				,	"move@2": ("link", "move_transport")
				,	"move@3": ("link", "move_body")
				,	"move@4": ("link", "move_body")
				}

	def show(self):
		print "PARAMETERS:"
		print vars(self)

	def end(self):
		self.show()
