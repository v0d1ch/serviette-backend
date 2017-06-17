# SERVIETTE - Yesod Mysql example 
  [Serviette](https://hackage.haskell.org/package/serviette) is library for generating SQL queries from JSON. 
  Send the json in the expected format and receive raw sql string.

### Expected JSON format

````
{
	"format":1,
    "action":"SELECT",
    "selectName": "users",
    "joinTables":[
    	  {"tablename":"addresses","field":"userid","operator":"=","withTable":"users", "withField":"id"},
          {"tablename":"posts","field":"userid","operator":"=","withTable":"users", "withField":"id"}
    	],
    "whereCondition":[
          {"whereTableName":"users","whereField":"id", "whereOperator":">", "whereFieldValue": 1}
      ]
}
````

If `format` is set to 1 you will get raw sql string back:

````
SELECT users join addresses on userid = users.id join posts on userid = users.id where users.id > 1
````

Take a look at `Handler/Api` for the implementation

