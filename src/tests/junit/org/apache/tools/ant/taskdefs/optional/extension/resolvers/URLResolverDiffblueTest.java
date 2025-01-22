package org.apache.tools.ant.taskdefs.optional.extension.resolvers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.extension.Extension;
import org.junit.Test;

public class URLResolverDiffblueTest {
  /**
   * Test {@link URLResolver#resolve(Extension, Project)}.
   * <p>
   * Method under test: {@link URLResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve() throws MalformedURLException, BuildException {
    // Arrange
    URLResolver urlResolver = new URLResolver();
    urlResolver.setUrl(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResolver.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    urlResolver.setDestfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResolver.resolve(extension, new Project()));
  }

  /**
   * Test {@link URLResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link URLResolver} (default constructor) Destfile is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenURLResolverDestfileIsNull_thenThrowBuildException()
      throws MalformedURLException, BuildException {
    // Arrange
    URLResolver urlResolver = new URLResolver();
    urlResolver.setUrl(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResolver.setDestdir(null);
    urlResolver.setDestfile(null);
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResolver.resolve(extension, new Project()));
  }

  /**
   * Test {@link URLResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link URLResolver} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenURLResolver_thenThrowBuildException() throws BuildException {
    // Arrange
    URLResolver urlResolver = new URLResolver();
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResolver.resolve(extension, new Project()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link URLResolver}
   *   <li>{@link URLResolver#setDestdir(File)}
   *   <li>{@link URLResolver#setDestfile(File)}
   *   <li>{@link URLResolver#setUrl(URL)}
   *   <li>{@link URLResolver#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws MalformedURLException {
    // Arrange and Act
    URLResolver actualUrlResolver = new URLResolver();
    actualUrlResolver.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualUrlResolver.setDestfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualUrlResolver.setUrl(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    String actualToStringResult = actualUrlResolver.toString();

    // Assert
    assertEquals(
        String.join("", "URL[file:",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator), "]"),
        actualToStringResult);
  }
}
