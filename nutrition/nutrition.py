#!/usr/bin/env python3

import argparse
import json
import math
import urllib3
import urllib
import yaml

APP_ID  = 'fac90f20'
APP_KEY = 'b35671c6cf9007c984b838074222c83c'

class Nutrient:
    __prefixes = [
        ('M', 1e+6),
        ('k', 1e+3),
        ('',  1e+0),
        ('m', 1e-3),
        ('µ', 1e-6),
    ]
    __units = [
        'g',
        'cal',
    ]

    def __init__(self, name, quantity, unit):
        self.name = name
        self.unit = None

        for u in Nutrient.__units:
            if unit[-len(u):] == u:
                self.unit = u
                prefix = unit[:-len(u)]
                self.quantity = float(quantity) * self.prefixValue(prefix)
                break

        if self.unit == None:
            raise Exception('Unknown unit: %s' % unit)

    def __add__(first, second):
        if first.name != second.name:
            raise Exception('Nutrient mismatch: %s vs %s'
                            % (first.name, second.name))
        return Nutrient(first.name,
                        first.quantity + second.quantity,
                        first.unit)

    def __repr__(self):
        prefixName,prefixValue = self.fittingPrefix()
        unit = prefixName + self.unit
        value = self.quantity / prefixValue
        return '%s (%.4f %s)' % (self.name, value, unit)

    def prefixValue(self, prefix):
        for name, value in self.__prefixes:
            if prefix == name:
                return value
        raise Exception('Unknown prefix: (%s)' % str(prefix))

    def fittingPrefix(self):
        '''Finds a good prefix for this nutrient'''
        if self.quantity == 0:
            return ('', 1)
        for i in range(len(self.__prefixes)):
            name, value = self.__prefixes[i]
            if math.floor(self.quantity / value) > 0:
                return (name, value)
        return self.__prefixes[-1]

    def quantityForPrefix(self, prefix):
        return self.quantity * self.__prefixes[prefix]

    def toCleanStr(self):
        prefixName,prefixValue = self.fittingPrefix()
        value = self.quantity / prefixValue
        return '%8.2f %s%s' % (value, prefixName, self.unit)

class Food:
    API_URL = "https://api.edamam.com/api/nutrition-data"
    HTTP = urllib3.PoolManager()

    def __init__(self, ingredients : list, nutrients : dict, calories : float):
        self.ingredients = ingredients
        self.nutrients = nutrients
        self.calories = calories

    def _validateResponse(response, data):
        return response.status == 200 and \
            len(data['ingredients']) == 1 and \
            data['totalNutrients']

    def _parseIngredients(data):
        ingredients = []
        for ing in data['ingredients']:
            ing = ing['parsed'][0]
            ingredients.append('%2d: %s' % (ing['quantity'], ing['food']))
        return ingredients

    def _parseNutrients(data):
        nutrients = {}
        for n in data['totalNutrients'].values():
            nutrient = Nutrient(n['label'], n['quantity'], n['unit'])

            if nutrient.name in nutrients.keys():
                nutrients[nutrient.name] += nutrient
            else:
                nutrients[nutrient.name] = nutrient
        return nutrients

    def __add__(first, second):
        nutrients = first.nutrients
        ingredients = first.ingredients + second.ingredients
        calories = first.calories + second.calories

        for nutrient in second.nutrients.values():
            if nutrient.name in nutrients.keys():
                nutrients[nutrient.name] += nutrient
            else:
                nutrients[nutrient.name] = nutrient

        return Food(ingredients, nutrients, calories)

    def fromIngredient(query):
        API_ARGS = "?app_id=%s&app_key=%s&ingr=" % (APP_ID, APP_KEY)
        url = Food.API_URL + API_ARGS + urllib.parse.quote(query)
        r = Food.HTTP.request('GET', url)
        data = json.loads(r.data.decode('ISO-8859-1'))

        if not Food._validateResponse(r, data):
            print(r.getheaders())
            print(json.dumps(data, indent=4, sort_keys=True))
            return None

        return Food(Food._parseIngredients(data),
                    Food._parseNutrients(data),
                    float(data['calories']))

    def emptyPlate():
        # :)
        return Food([], {}, 0.0)


parser = argparse.ArgumentParser()
parser.add_argument('--requirements', dest='requirementsPath',
        type=str, default='requirements.yaml')
parser.add_argument('--foods', dest='foodsPath',
        type=str, default='foods.yaml')
args = parser.parse_args()

with open(args.foodsPath) as f:
    foods = yaml.load(f.read())
with open(args.requirementsPath) as f:
    requirements = yaml.load(f.read())

allFood = Food.emptyPlate()
for foodName in foods['ingredient']:
    allFood += Food.fromIngredient(foodName)

print('\nIngredients:\n%s' % '\n'.join(allFood.ingredients))
print('\nCalories: %d' % allFood.calories)
print('\nNutrients: ')

for name in sorted(allFood.nutrients.keys()):
    title = name.ljust(20)
    value = allFood.nutrients[name].toCleanStr()
    print('%s\t %s' % (title, value))

