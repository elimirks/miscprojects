package game

import javax.swing.JFrame

class Game extends JFrame {
	add(new Board())

	setSize(512, 512)

	setTitle("Game")
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setLocationRelativeTo(null)
}

