#!/usr/bin/env python3

# The weighting algorithm will need to be randomly mutating... hm.

# TODO: use sigmoids for synapses
class Synapse:
    def __init__(self, inputEndpoint):
        self.weight = 0
        self._inputEndpoint = inputEndpoint

    def getValue(self):
        return self._inputEndpoint.getAxonValue()

class Node:
    # Returns a value in [0,1]
    def getAxonValue(self):
        return 0.5

class InputNode(Node):
    def __init__(self, value):
        self.value = value

    def getAxonValue(self):
        return self.value

# https://psychology.stackexchange.com/questions/9144/how-many-dendrite-connections-vs-axon-terminals-does-a-multipolar-cerebral-neuro
# Many inputs, one output (but the output could feed to multiple other nodes)
class WeightingNode(Node):
    def __init__(self):
        self._inputSynapses = []

    def addInputSynapse(self, s):
        self._inputSynapses.append(s)

    def _calculateInputValue(self):
        netInputWeight = 0
        netInputValues = 0

        for i in self.inputs:
            netInputWeight += i.weight
            netInputValues += i.getValue() / i.weight

        return netInputValues / netInputWeight

    def getAxonValue(self):
        return self._calculateInputValue()

# Have to keep track of them for ticking
allMemoryNodes = []

# http://www.human-memory.net/processes_storage.html
# MemoryNodes thus should be interspersed among NeuronNodes
# Working memory node, that is.
class MemoryWeightingNode(WeightingNode):
    def __init__(self):
        WeightingNode.__init__(self)
        self.value = 0
        allMemoryNodes.append(self)

    def tick(self):
        inputValue = self._calculateInputValue()
        self.value = self.value * 0.9 + inputValue * 0.1

    def getAxonValue(self):
        return self.value

inputNodes = []
