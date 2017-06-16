# SERVIETTE - Yesod Mysql example 
  Serviette is library for generating SQL queries from JSON. 
  Send the json in the expected format and receive raw sql string.

### Expected JSON format
````
{
   "sql":{
      "format":1,
      "command":"SELECT",
      "selectName":"users",
      "join":[
         {
            "tableName":"contracts",
            "field":"contractField",
            "operator":">",
            "withTable":"users",
            "withField":"usersField"
         },
         {
            "tableName":"commissions",
            "field":"contractField",
            "operator":"<",
            "withTable":"contracts",
            "withField":"usersField"
         }
      ],
      "whereCondition":[
         {
            "tableName":"commissions",
            "field":"contractField",
            "operator":"like",
            "fieldValue":1
         }
      ]
   }
}
````
