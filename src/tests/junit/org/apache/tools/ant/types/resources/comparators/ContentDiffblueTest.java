package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.LogOutputResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.junit.Test;

public class ContentDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Content#setBinary(boolean)}
   *   <li>{@link Content#isBinary()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Content content = new Content();

    // Act
    content.setBinary(true);

    // Assert
    assertTrue(content.isBinary());
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare() {
    // Arrange
    Content content = new Content();
    LogOutputResource foo = new LogOutputResource(Path.systemBootClasspath, 1);

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare2() {
    // Arrange
    Content content = new Content();
    LogOutputResource foo = new LogOutputResource(Path.systemBootClasspath, 1);

    // Act and Assert
    assertEquals(0, content.resourceCompare(foo, new LogOutputResource(Path.systemBootClasspath, 1)));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenFileAttributeIsNull() {
    // Arrange
    Content content = new Content();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_givenTrue_whenResourceDirectoryIsTrue_thenReturnZero() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Resource bar = new Resource();
    bar.setDirectory(true);

    // Act and Assert
    assertEquals(0, content.resourceCompare(foo, bar));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_thenReturnMinusOne() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_thenReturnOne() {
    // Arrange
    Content content = new Content();
    Resource foo = new Resource();

    // Act and Assert
    assertEquals(1, content.resourceCompare(foo,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_thenReturnZero() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(0, content.resourceCompare(foo,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenFileResource_thenReturnZero() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource();

    // Act and Assert
    assertEquals(0, content.resourceCompare(foo, new FileResource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenJavaConstantResource_thenReturnMinusOne() {
    // Arrange
    Content content = new Content();
    JavaConstantResource foo = new JavaConstantResource();

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link FileResource#FileResource(File)} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenMappedResourceWithRIsFileResourceAndMIsCutDirsMapper() {
    // Arrange
    Content content = new Content();
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource foo = new MappedResource(r, new CutDirsMapper());

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource()));
  }

  /**
   * Test {@link Content#resourceCompare(Resource, Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String, boolean, long)} with {@code Name} and exists is {@code true} and lastmodified is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Content#resourceCompare(Resource, Resource)}
   */
  @Test
  public void testResourceCompare_whenResourceWithNameAndExistsIsTrueAndLastmodifiedIsOne() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(-1, content.resourceCompare(foo, new Resource("Name", true, 1L)));
  }

  /**
   * Test new {@link Content} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Content}
   */
  @Test
  public void testNewContent() {
    // Arrange and Act
    Content actualContent = new Content();

    // Assert
    Location location = actualContent.getLocation();
    assertNull(location.getFileName());
    assertNull(actualContent.getDescription());
    assertNull(actualContent.getProject());
    assertNull(actualContent.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualContent.isReference());
    assertTrue(actualContent.isBinary());
  }
}
