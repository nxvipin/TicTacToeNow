var TTT = Backbone.Model.extend({
	defaults: {
		current_player: 'X',
		board: ['*', '*', '*', '*', '*', '*', '*', '*', '*']
	},
	validate: function(new_board){
		return true;
	},
	initialize: function(){
		console.log('Game state initialized');
		this.on('change:board', function(){
				console.log('Board state changed');
		});
		this.on('change:current_player', function(){
			console.log('Current player changed');
		});
	},
	get_board: function(){
		return this.get('board');
	},
	get_current_player: function(){
		return this.get('current_player');
	},
	update_current_player: function(player){
		this.set('current_player', player);
		return true;
	},
	get_valid_moves: function(){
		board = this.get('board');
		valid_moves = board.map(function(e,i,arr){
						if(e === "*")
							return i;
						else
							return -1;
						})
						.filter(function(e,i,arr){
						if(e>=0)
							return true;
						else
							return false;
						});
		return valid_moves;
	},
	toggle_current_player: function(){
		cp = this.get_current_player();
		np = "";
		if(cp==='X'){ np = 'O'} else { np = 'X'};
		this.update_current_player(np);
		return true;
	},
	move: function(location){
		if(this.get_valid_moves().indexOf(location)>=0){
			board = this.get('board');
			board[location] = this.get_current_player();
			this.set('board', board);
			this.toggle_current_player();
			return board;
		}
		else{
			console.log("Invalid Move");
			return {error: "Invalid Move"};
		}
	}
});

var succ = function(game){
	console.log(game.get('current_player'));
}
var game = new TTT();

var BOARD = Backbone.View.extend({
	board_template: _.template($('#board').html()),
	initialize : function() {
		this.render();
		this.listenTo(this.model, 'change', this.render);
	},
	el : $('#game'),
	render : function(){
		console.log("render");
		console.log(this.board_template(board=this.model.get_board() ));
		this.$el.html( this.board_template(board=this.model.get_board() ) );
		return this;
	}
});

var board_render = new BOARD({model: game});

