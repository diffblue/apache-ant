package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class ExistsDiffblueTest {
  /**
   * Test {@link Exists#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenFileAttributeIsNull_thenReturnMinusOne() {
    // Arrange
    Exists exists = new Exists();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(-1, exists.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Exists#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResource_thenReturnZero() {
    // Arrange
    Exists exists = new Exists();
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, exists.resourceCompare(foo, new Resource()));
  }

  /**
   * Test new {@link Exists} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Exists}
   */
  @Test
  public void testNewExists() {
    // Arrange and Act
    Exists actualExists = new Exists();

    // Assert
    Location location = actualExists.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExists.getDescription());
    assertNull(actualExists.getProject());
    assertNull(actualExists.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualExists.isReference());
  }
}
