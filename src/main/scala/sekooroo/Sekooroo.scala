package sekooroo

object Sekooroo {


  trait QOperator
  case object Eq extends QOperator
  case object NEq extends QOperator
  case object Gt  extends QOperator
  case object Lt  extends QOperator
  case object GtEq extends QOperator
  case object LtEq extends QOperator
  
  trait ArrayQOperator
  case object In extends ArrayQOperator
  case object NotIn extends ArrayQOperator


  trait SingleSetQueryCondition[A]{
  
    def toSqlString: String
  }
  case class SingleQueryCondition[A](column: Column[A],operator: QOperator,value: A) extends SingleSetQueryCondition[A]{
  
    override def toSqlString: String = {
    
      operator match {
  
        case Eq   => this.column.colName + " = "  + value
        case NEq  => this.column.colName + " != " + value
        case Lt   => this.column.colName + " < "  + value
        case Gt   => this.column.colName + " > "  + value
        case LtEq => this.column.colName + " <= " + value
        case GtEq => this.column.colName + " >= " + value
      }
    }
  }
  case class MultiQueryCondition[A](column: Column[A],operator: ArrayQOperator,values: Seq[A]) extends SingleSetQueryCondition[A] {
  
    override def toSqlString: String = {
    
      operator match {
        
        case In    => " in " + values.reduceLeftOption(_ + ", " + _).getOrElse("false")
        case NotIn => " not in " + values.reduceLeftOption(_ + ", " + _).getOrElse("true")
      }
    
    }
  }

  trait QCondition[A]
  case class QueryCondition(conditions: Seq[SingleSetQueryCondition[_]]){
  
    def and(other: SingleSetQueryCondition[_]): QueryCondition = this.copy(conditions = conditions :+ other)
    
    def and(other: QueryCondition): QueryCondition = this.copy(conditions = conditions ++ other.conditions)
  }
  
  
  case class ArrayQueryCondition[A](colName: String, operator: String, value: Seq[A]) extends QCondition[A]
  
  
  
  class Column[A](val colName: String){
  
    def ===   (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,Eq, value)))
    def !==   (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,NEq,value)))
    def >     (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,Gt,value)))
    def >=    (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,GtEq,value)))
    def <     (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,Lt,value)))
    def <=    (value: A ) = QueryCondition(Seq(SingleQueryCondition(this,LtEq,value)))
    def in    (value: A*) = QueryCondition(Seq(MultiQueryCondition(this,In,value)))
    def notIn (value: A*) = QueryCondition(Seq(MultiQueryCondition(this,NotIn,value)))
  }
  
  object Column {
  
    def apply[A](name: String) = new Column[A](name)
  }
  
  class Table(val tableName: String)
  
  case class SelectQuery[T <: Table](
    table: T,
    selects: Seq[Column[_]]     = Seq(),
    where:   Option[QueryCondition] = None
  ){
  
    var _select: Option[List[Column[_]]] = None
  
    def select(func: T => Seq[Column[_]]): SelectQuery[T] = {
    
      this.copy(selects = func.apply(this.table))
    }
    
    def where(func: T => QueryCondition): SelectQuery[T] = {
      
      this.copy(where = Some(func.apply(this.table)))
    }
    
    def whereString: String =
      where.map { qCondition =>
        qCondition.conditions.reduceLeftOption(_.toSqlString + " and " + _.toSqlString).getOrElse("")
      }
      .getOrElse("")
    
    def toSql: String = "select " + this.selects.reduceLeftOption(_ + "," +_).getOrElse("*") + " from " + this.table.tableName + whereString
  }
  
  object from {
  
    def apply[T <: Table](table: T): SelectQuery[T] =
      SelectQuery[T](table = table)
  }
  
  
  //Example
  
  class MyTable extends Table("my_table") {
  
    def id          = Column[Int]("id")
    def name        = Column[String]("name")
    def age         = Column[Int]("age")
    def moneyInBank = Column[Double]("money_in_bank")
    
    def * = Seq(id,name)
  }
  
  val myTable = new MyTable
  
  
  def query: String =
    from(myTable)
    .select(t => Seq(t.id,t.name))
    .where(t => t.name === "time" and t.id === 43 and t.moneyInBank > 44.00)
    .toSql
  
}
