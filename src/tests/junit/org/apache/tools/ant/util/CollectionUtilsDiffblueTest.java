package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.Vector;
import org.apache.maven.resolver.internal.ant.org.apache.http.message.BasicHeaderElementIterator;
import org.apache.maven.resolver.internal.ant.org.apache.http.message.BasicListHeaderIterator;
import org.junit.Test;

public class CollectionUtilsDiffblueTest {
  /**
  * Method under test: {@link CollectionUtils#asCollection(Iterator)}
  */
  @Test
  public void testAsCollection() {
    // Arrange, Act and Assert
    assertTrue(CollectionUtils.asCollection(
        new BasicHeaderElementIterator(new BasicListHeaderIterator(new ArrayList<>(), "https://example.org/example")))
        .isEmpty());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>default or parameterless constructor of {@link CollectionUtils.EmptyEnumeration}
   *   <li>{@link CollectionUtils.EmptyEnumeration#hasMoreElements()}
   * </ul>
   */
  @Test
  public void testEmptyEnumerationConstructor() {
    // Arrange, Act and Assert
    assertFalse((new CollectionUtils.EmptyEnumeration<>()).hasMoreElements());
  }

  /**
   * Method under test: {@link CollectionUtils.EmptyEnumeration#nextElement()}
   */
  @Test
  public void testEmptyEnumerationNextElement() throws NoSuchElementException {
    // Arrange, Act and Assert
    assertThrows(NoSuchElementException.class, () -> (new CollectionUtils.EmptyEnumeration<>()).nextElement());
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals() {
    // Arrange
    Hashtable<Object, Object> d1 = new Hashtable<>(1);

    // Act and Assert
    assertTrue(CollectionUtils.equals(d1, new Hashtable<>(1)));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals2() {
    // Arrange, Act and Assert
    assertTrue(CollectionUtils.equals((Dictionary<?, ?>) null, null));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals3() {
    // Arrange
    Hashtable<Object, Object> objectObjectMap = new Hashtable<>(1);
    objectObjectMap.put("42", "42");

    // Act and Assert
    assertFalse(CollectionUtils.equals(objectObjectMap, new Hashtable<>(1)));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals4() {
    // Arrange
    Hashtable<Object, Object> objectObjectMap = new Hashtable<>(1);
    objectObjectMap.put("42", "42");

    Hashtable<Object, Object> objectObjectMap1 = new Hashtable<>(1);
    objectObjectMap1.put("42", "42");

    // Act and Assert
    assertTrue(CollectionUtils.equals(objectObjectMap, objectObjectMap1));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals5() {
    // Arrange
    Hashtable<Object, Object> objectObjectMap = new Hashtable<>(1);
    objectObjectMap.put("42", "42");

    Hashtable<Object, Object> objectObjectMap1 = new Hashtable<>(1);
    objectObjectMap1.put(1, "42");

    // Act and Assert
    assertFalse(CollectionUtils.equals(objectObjectMap, objectObjectMap1));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals6() {
    // Arrange, Act and Assert
    assertFalse(CollectionUtils.equals(new Hashtable<>(), (Dictionary<?, ?>) null));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Dictionary, Dictionary)}
   */
  @Test
  public void testEquals7() {
    // Arrange, Act and Assert
    assertFalse(CollectionUtils.equals((Dictionary<?, ?>) null, new Hashtable<>()));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEquals8() {
    // Arrange
    Vector<String> v1 = JavaEnvUtils.getJrePackageTestCases();

    // Act and Assert
    assertTrue(CollectionUtils.equals(v1, JavaEnvUtils.getJrePackageTestCases()));
  }

  /**
   * Method under test: {@link CollectionUtils#equals(Vector, Vector)}
   */
  @Test
  public void testEquals9() {
    // Arrange
    Stack<Object> v1 = new Stack<>();

    // Act and Assert
    assertFalse(CollectionUtils.equals(v1, JavaEnvUtils.getJrePackageTestCases()));
  }

  /**
   * Method under test: {@link CollectionUtils#flattenToString(Collection)}
   */
  @Test
  public void testFlattenToString() {
    // Arrange, Act and Assert
    assertEquals("", CollectionUtils.flattenToString(new ArrayList<>()));
  }

  /**
   * Method under test: {@link CollectionUtils#frequency(Collection, Object)}
   */
  @Test
  public void testFrequency() {
    // Arrange, Act and Assert
    assertEquals(0, CollectionUtils.frequency(new ArrayList<>(), "42"));
    assertEquals(0, CollectionUtils.frequency(null, "42"));
  }

  /**
   * Method under test: {@link CollectionUtils#putAll(Dictionary, Dictionary)}
   */
  @Test
  public void testPutAll() {
    // Arrange
    Hashtable<Object, Object> objectObjectMap = new Hashtable<>(1);

    Hashtable<Object, Object> objectObjectMap1 = new Hashtable<>(1);
    objectObjectMap1.put("42", "42");

    // Act
    CollectionUtils.putAll(objectObjectMap, objectObjectMap1);

    // Assert
    assertEquals(1, objectObjectMap.size());
  }
}

