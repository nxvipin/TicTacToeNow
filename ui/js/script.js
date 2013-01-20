var TTT = Backbone.Model.extend({
	defaults: {
		play: true,
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
	check_valid_move: function(move){
		if(this.get_valid_moves().indexOf(move)>=0){
			return true;
		} else {
			return false;
		}
	},
	toggle_current_player: function(){
		cp = this.get_current_player();
		np = "";
		if(cp==='X'){ np = 'O'} else { np = 'X'};
		this.update_current_player(np);
		return true;
	},
	move: function(move){
		if(this.get('play') && this.check_valid_move(move)){
			board = this.get('board');
			board[move] = this.get_current_player();
			this.set('board', board);
			this.toggle_current_player();
			return board;
		}
		else{
			console.log("Invalid Move");
			return {error: "Invalid Move"};
		}
	},
	win: function(sym){
		board = this.get_board();
		if(
			board[0] == sym && board[1] == sym && board[2] == sym ||
			board[3] == sym && board[4] == sym && board[5] == sym ||
			board[6] == sym && board[7] == sym && board[8] == sym ||
			board[0] == sym && board[3] == sym && board[6] == sym ||
			board[1] == sym && board[4] == sym && board[7] == sym ||
			board[2] == sym && board[5] == sym && board[8] == sym ||
			board[0] == sym && board[4] == sym && board[8] == sym ||
			board[2] == sym && board[4] == sym && board[6] == sym){
				return true;
		} else {
			return false;
		}
	},
	over: function(){
		board = this.get_board();
		b = board.filter(function(X){ if(X=='*') return true;});
		if(b.length==0){
			return true;
		} else{
			return false;
		}
	}
});

var succ = function(game){
	console.log(game.get('current_player'));
}
var game = new TTT();
game.on("change", function(){
	console.log("Game Changes Captured");
	if(this.win('X') || this.win('O') || this.over()){
		this.set('play',false);
	}
	if(this.hasChanged('play')){
		console.log("Game Over!");
	}
});

var BOARD = Backbone.View.extend({
	board_template: _.template($('#board').html()),
	initialize : function() {
		this.render();
		this.listenTo(this.model, 'change', this.render);
	},
	el : $('#game'),
	events: {'click .cell': function(X){
		console.log('Clicked Cell: ',X.currentTarget.id);
		this.model.move(parseInt(X.currentTarget.id[1]));
	}},
	render : function(){
		console.log("render");
		this.$el.html( this.board_template(board=this.model.get_board() ) );
		return this;
	}
});

var board_render = new BOARD({model: game});

