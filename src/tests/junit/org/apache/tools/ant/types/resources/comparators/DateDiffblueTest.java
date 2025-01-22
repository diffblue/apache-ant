package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class DateDiffblueTest {
  /**
   * Test {@link Date#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenFileAttributeIsNull() {
    // Arrange
    Date date = new Date();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(0, date.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Date#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResource_thenReturnZero() {
    // Arrange
    Date date = new Date();
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, date.resourceCompare(foo, new Resource()));
  }

  /**
   * Test new {@link Date} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Date}
   */
  @Test
  public void testNewDate() {
    // Arrange and Act
    Date actualDate = new Date();

    // Assert
    Location location = actualDate.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDate.getDescription());
    assertNull(actualDate.getProject());
    assertNull(actualDate.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualDate.isReference());
  }
}
