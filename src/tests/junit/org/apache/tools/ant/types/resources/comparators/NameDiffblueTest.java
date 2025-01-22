package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class NameDiffblueTest {
  /**
   * Test {@link Name#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return twenty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResourceWithName_thenReturnTwentyFour() {
    // Arrange
    Name name = new Name();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(24, name.resourceCompare(foo, new Resource("Name")));
  }

  /**
   * Test new {@link Name} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Name}
   */
  @Test
  public void testNewName() {
    // Arrange and Act
    Name actualName = new Name();

    // Assert
    Location location = actualName.getLocation();
    assertNull(location.getFileName());
    assertNull(actualName.getDescription());
    assertNull(actualName.getProject());
    assertNull(actualName.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualName.isReference());
  }
}
