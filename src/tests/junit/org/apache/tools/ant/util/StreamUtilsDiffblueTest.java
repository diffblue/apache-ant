package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Test;

public class StreamUtilsDiffblueTest {
  /**
   * Test {@link StreamUtils#enumerationAsStream(Enumeration)}.
   * <p>
   * Method under test: {@link StreamUtils#enumerationAsStream(Enumeration)}
   */
  @Test
  public void testEnumerationAsStream() {
    // Arrange and Act
    Stream<Object> actualEnumerationAsStreamResult = StreamUtils.enumerationAsStream(new StringTokenizer("foo"));

    // Assert
    List<Object> collectResult = actualEnumerationAsStreamResult.limit(5).collect(Collectors.toList());
    assertEquals(1, collectResult.size());
    assertEquals("foo", collectResult.get(0));
  }

  /**
   * Test {@link StreamUtils#iteratorAsStream(Iterator)}.
   * <p>
   * Method under test: {@link StreamUtils#iteratorAsStream(Iterator)}
   */
  @Test
  public void testIteratorAsStream() {
    // Arrange
    ArrayList<Object> objectList = new ArrayList<>();

    // Act
    Stream<Object> actualIteratorAsStreamResult = StreamUtils.iteratorAsStream(objectList.iterator());

    // Assert
    assertTrue(actualIteratorAsStreamResult.limit(5).collect(Collectors.toList()).isEmpty());
  }
}
