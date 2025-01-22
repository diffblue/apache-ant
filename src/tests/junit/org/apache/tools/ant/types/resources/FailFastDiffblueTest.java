package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class FailFastDiffblueTest {
  /**
   * Test {@link FailFast#remove()}.
   * <p>
   * Method under test: {@link FailFast#remove()}
   */
  @Test
  public void testRemove() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();

    // Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new FailFast("42", resourceList.iterator())).remove());
  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then not {@link ArrayList#ArrayList()} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_givenResource_thenNotArrayListIteratorHasNext() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    Resource resource = new Resource();
    resourceList.add(resource);
    Iterator<Resource> i = resourceList.iterator();

    // Act
    FailFast actualFailFast = new FailFast("42", i);

    // Assert
    Resource actualNextResult = actualFailFast.next();
    boolean actualHasNextResult = actualFailFast.hasNext();
    assertFalse(i.hasNext());
    assertFalse(actualHasNextResult);
    assertSame(resource, actualNextResult);
  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>Then not {@link FailFast#FailFast(Object, Iterator)} with o is {@code 42} and i is {@link ArrayList#ArrayList()} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_thenNotFailFastWithOIs42AndIIsArrayListIteratorHasNext() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    FailFast i = new FailFast("42", resourceList.iterator());

    // Act and Assert
    assertFalse((new FailFast("42", i)).hasNext());
    assertFalse(i.hasNext());
  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>Then not {@link FailFast#FailFast(Object, Iterator)} with o is {@code 42} and i is {@link ArrayList#ArrayList()} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_thenNotFailFastWithOIs42AndIIsArrayListIteratorHasNext2() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    Resource resource = new Resource();
    resourceList.add(resource);
    FailFast i = new FailFast("42", resourceList.iterator());

    // Act
    FailFast actualFailFast = new FailFast("42", i);

    // Assert
    Resource actualNextResult = actualFailFast.next();
    assertFalse(actualFailFast.hasNext());
    assertFalse(i.hasNext());
    assertSame(resource, actualNextResult);
  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()} iterator.</li>
   *   <li>Then not {@link ArrayList#ArrayList()} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_whenArrayListIterator_thenNotArrayListIteratorHasNext() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    Iterator<Resource> i = resourceList.iterator();

    // Act
    FailFast actualFailFast = new FailFast("42", i);

    // Assert
    assertFalse(i.hasNext());
    assertFalse(actualFailFast.hasNext());
  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new FailFast(null, null));

  }

  /**
   * Test {@link FailFast#FailFast(Object, Iterator)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#FailFast(Object, Iterator)}
   */
  @Test
  public void testNewFailFast_whenNull_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new FailFast("42", null));

  }

  /**
   * Test {@link FailFast#hasNext()}.
   * <ul>
   *   <li>Given {@link ArrayList#ArrayList()} add {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#hasNext()}
   */
  @Test
  public void testHasNext_givenArrayListAddResource_thenReturnTrue() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    resourceList.add(new Resource());

    // Act and Assert
    assertTrue((new FailFast("42", resourceList.iterator())).hasNext());
  }

  /**
   * Test {@link FailFast#hasNext()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#hasNext()}
   */
  @Test
  public void testHasNext_thenReturnFalse() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();

    // Act and Assert
    assertFalse((new FailFast("42", resourceList.iterator())).hasNext());
  }

  /**
   * Test {@link FailFast#next()}.
   * <ul>
   *   <li>Then not {@link FailFast#FailFast(Object, Iterator)} with o is {@code 42} and i is {@link ArrayList#ArrayList()} iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#next()}
   */
  @Test
  public void testNext_thenNotFailFastWithOIs42AndIIsArrayListIteratorHasNext() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();
    Resource resource = new Resource();
    resourceList.add(resource);
    FailFast failFast = new FailFast("42", resourceList.iterator());

    // Act
    Resource actualNextResult = failFast.next();

    // Assert
    assertFalse(failFast.hasNext());
    assertSame(resource, actualNextResult);
  }

  /**
   * Test {@link FailFast#next()}.
   * <ul>
   *   <li>Then throw {@link NoSuchElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FailFast#next()}
   */
  @Test
  public void testNext_thenThrowNoSuchElementException() {
    // Arrange
    ArrayList<Resource> resourceList = new ArrayList<>();

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> (new FailFast("42", resourceList.iterator())).next());
  }
}
