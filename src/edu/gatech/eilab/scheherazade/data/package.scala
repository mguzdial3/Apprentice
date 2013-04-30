package edu.gatech.eilab.scheherazade

package object data {

  import com.thoughtworks.xstream.converters._
  import com.thoughtworks.xstream.converters.collections._
  import com.thoughtworks.xstream._
  import com.thoughtworks.xstream.mapper._
  import com.thoughtworks.xstream.io._
  import com.thoughtworks.xstream.annotations._
  import sevenzip.{SevenZip => SZip}

  val XStream = new XStream()
  ListConverter.configureXStream(XStream)
  XStream.alias("Sentence", classOf[Sentence]);
  XStream.alias("Story", classOf[Story]);
  XStream.alias("Token", classOf[Token]);
  XStream.alias("Dependency", classOf[Dependency]);
  
  val SevenZip = new SZip()
}

package data {
  import com.thoughtworks.xstream.converters._
  import com.thoughtworks.xstream.converters.collections._
  import com.thoughtworks.xstream._
  import com.thoughtworks.xstream.mapper._
  import com.thoughtworks.xstream.io._

  /** used to make the XML generated by XStream for Scala Lists readable
   * 
   */
  class ListConverter(_mapper: Mapper) extends AbstractCollectionConverter(_mapper) {
    def canConvert(clazz: Class[_]) = {
      // "::" is the name of the list class, also handle nil
      //println("passed in " + clazz)
      classOf[::[_]] == clazz || Nil.getClass == clazz //|| classOf[List[_]] == clazz
    }

    def marshal(value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
      val list = value.asInstanceOf[List[_]]
      for (item <- list) {
        writeItem(item, context, writer)
      }
    }

    def unmarshal(reader: HierarchicalStreamReader, context: UnmarshallingContext) = {
      var list: List[_] = Nil
      while (reader.hasMoreChildren()) {
        reader.moveDown();
        val item = readItem(reader, context, list);
        list = list ::: List(item) // be sure to build the list in the same order  
        reader.moveUp();
      }
      list
    }
  }

  object ListConverter {
    def configureXStream(stream: XStream) = {
      stream.alias("list", classOf[::[_]])
      stream.alias("list", Nil.getClass)
      stream.registerConverter(new ListConverter(stream.getMapper))
    }
  }
}