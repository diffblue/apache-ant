package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class SizeDiffblueTest {
  /**
   * Test {@link Size#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Size#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenFileAttributeIsNull_thenReturnOne() {
    // Arrange
    Size size = new Size();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(1, size.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Size#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Size#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResource_thenReturnZero() {
    // Arrange
    Size size = new Size();
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, size.resourceCompare(foo, new Resource()));
  }

  /**
   * Test new {@link Size} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Size}
   */
  @Test
  public void testNewSize() {
    // Arrange and Act
    Size actualSize = new Size();

    // Assert
    Location location = actualSize.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSize.getDescription());
    assertNull(actualSize.getProject());
    assertNull(actualSize.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSize.isReference());
  }
}
