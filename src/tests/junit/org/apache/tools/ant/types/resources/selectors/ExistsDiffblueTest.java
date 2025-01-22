package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.StringResource;
import org.junit.Test;

public class ExistsDiffblueTest {
  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Exists exists = new Exists();

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(exists.isSelected(r));
  }

  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Exists is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenTrue_whenResourceExistsIsTrue_thenReturnTrue() {
    // Arrange
    Exists exists = new Exists();

    Resource r = new Resource();
    r.setExists(true);

    // Act and Assert
    assertTrue(exists.isSelected(r));
  }

  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Exists exists = new Exists();
    Resource r = new Resource();

    // Act and Assert
    assertTrue(exists.isSelected(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenResourceWithName_thenReturnFalse() {
    // Arrange
    Exists exists = new Exists();

    // Act and Assert
    assertFalse(exists.isSelected(new Resource("Name")));
  }

  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenResource_thenReturnTrue() {
    // Arrange
    Exists exists = new Exists();

    // Act and Assert
    assertTrue(exists.isSelected(new Resource()));
  }

  /**
   * Test {@link Exists#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link StringResource#StringResource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Exists#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenStringResource_thenReturnFalse() {
    // Arrange
    Exists exists = new Exists();

    // Act and Assert
    assertFalse(exists.isSelected(new StringResource()));
  }
}
