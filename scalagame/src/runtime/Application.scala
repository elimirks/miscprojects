package runtime

import java.awt.EventQueue

import game.Game

object Application {
	def main(args: Array[String]) {
		EventQueue.invokeLater(new Runnable() {
			@Override
			def run() {
				val ex = new Game
				ex.setVisible(true)
			}
		});
	}
}

