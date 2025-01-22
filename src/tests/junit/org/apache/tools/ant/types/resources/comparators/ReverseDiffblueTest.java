package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ReverseDiffblueTest {
  /**
   * Test {@link Reverse#Reverse()}.
   * <p>
   * Method under test: {@link Reverse#Reverse()}
   */
  @Test
  public void testNewReverse() {
    // Arrange and Act
    Reverse actualReverse = new Reverse();

    // Assert
    Location location = actualReverse.getLocation();
    assertNull(location.getFileName());
    assertNull(actualReverse.getDescription());
    assertNull(actualReverse.getProject());
    assertNull(actualReverse.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualReverse.isReference());
  }

  /**
   * Test {@link Reverse#Reverse(ResourceComparator)}.
   * <p>
   * Method under test: {@link Reverse#Reverse(ResourceComparator)}
   */
  @Test
  public void testNewReverse2() {
    // Arrange and Act
    Reverse actualReverse = new Reverse(new Content());

    // Assert
    Location location = actualReverse.getLocation();
    assertNull(location.getFileName());
    assertNull(actualReverse.getDescription());
    assertNull(actualReverse.getProject());
    assertNull(actualReverse.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualReverse.isReference());
  }

  /**
   * Test {@link Reverse#add(ResourceComparator)}.
   * <ul>
   *   <li>Given {@link Reverse#Reverse(ResourceComparator)} with c is {@link Content} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reverse#add(ResourceComparator)}
   */
  @Test
  public void testAdd_givenReverseWithCIsContent_thenThrowBuildException() {
    // Arrange
    Reverse reverse = new Reverse(new Content());

    // Act and Assert
    assertThrows(BuildException.class, () -> reverse.add(new Content()));
  }

  /**
   * Test {@link Reverse#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@link Reverse#Reverse(ResourceComparator)} with c is {@link Date} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Reverse#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenReverseWithCIsDate_whenResource_thenReturnZero() {
    // Arrange
    Reverse reverse = new Reverse(new Date());
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, reverse.resourceCompare(foo, new Resource()));
  }
}
