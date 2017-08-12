#!/usr/bin/env python3

import argparse
import json
import urllib3
import urllib
import yaml

parser = argparse.ArgumentParser()
parser.add_argument('--requirements', dest='requirementsPath',
        type=str, default='requirements.yaml')
parser.add_argument('--foods', dest='foodsPath',
        type=str, default='foods.yaml')
args = parser.parse_args()

APP_ID  = 'fac90f20'
APP_KEY = 'b35671c6cf9007c984b838074222c83c'

http = urllib3.PoolManager()

def validateResponse(response, data):
    return response.status == 200 and \
        len(data['ingredients']) == 1 and \
        data['totalNutrients']
        
def getNutritionJson(query):
    API_URL = "https://api.edamam.com/api/nutrition-data"
    API_ARGS = "?app_id=%s&app_key=%s&ingr=" % (APP_ID, APP_KEY)
    r = http.request('GET', API_URL + API_ARGS + urllib.parse.quote(query))
    data = json.loads(r.data.decode('ISO-8859-1'))

    if not validateResponse(r, data):
        print(r.getheaders())
        print(json.dumps(data, indent=4, sort_keys=True))
        return None

    return data


foods = []
requirements = {}

with open(args.foodsPath) as f:
    foods = yaml.load(f.read())
with open(args.requirementsPath) as f:
    requirements = yaml.load(f.read())

ingredients = []
totalCalories = 0
totalNutrients = {}
unitsNutrients = {}
for food in foods:
    data = getNutritionJson(food)
    if data == None:
        print('Invalid food: ' + food)
        continue
    totalCalories += data['calories']

    for ingredient in data['ingredients']:
        ingredient = ingredient['parsed'][0]
        ingredients.append('%2d: %s' % (ingredient['quantity'], ingredient['food']))

    for nutrient in data['totalNutrients'].values():
        name = nutrient['label']
        quantity = nutrient['quantity']
        units = nutrient['unit']

        if name in totalNutrients.keys():
            if unitsNutrients[name] != units:
                print('Unit mismatch!')
                exit(1)
            totalNutrients[name] += quantity
        else:
            totalNutrients[name] = quantity
            unitsNutrients[name] = units

print('\nIngredients:\n%s' % '\n'.join(ingredients))
print('\nCalories: %d' % totalCalories)
print('\nNutrients: ')

for name in sorted(totalNutrients.keys()):
    title = name.ljust(20)
    value = '%8.2f %s' % (totalNutrients[name], unitsNutrients[name])
    if name in requirements.keys():
        percent = 100 * totalNutrients[name] / requirements[name]
        value = '%s%6.2f%%' % (value.ljust(20), percent)
    print('%s\t %s' % (title, value))
