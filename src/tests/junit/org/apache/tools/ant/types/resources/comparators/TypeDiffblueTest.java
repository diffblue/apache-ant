package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.junit.Test;

public class TypeDiffblueTest {
  /**
   * Test {@link Type#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenFileAttributeIsNull() {
    // Arrange
    Type type = new Type();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(0, type.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Type#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Type type = new Type();
    Resource r = new Resource();
    MappedResource foo = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertEquals(0, type.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Type#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResource_thenReturnZero() {
    // Arrange
    Type type = new Type();
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(0, type.resourceCompare(foo, new Resource()));
  }

  /**
   * Test new {@link Type} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Type}
   */
  @Test
  public void testNewType() {
    // Arrange and Act
    Type actualType = new Type();

    // Assert
    Location location = actualType.getLocation();
    assertNull(location.getFileName());
    assertNull(actualType.getDescription());
    assertNull(actualType.getProject());
    assertNull(actualType.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualType.isReference());
  }
}
