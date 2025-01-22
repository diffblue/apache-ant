package org.apache.tools.ant.taskdefs.optional.extension.resolvers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.extension.Extension;
import org.junit.Test;

public class AntResolverDiffblueTest {
  /**
   * Test {@link AntResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link AntResolver} (default constructor).</li>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenAntResolver_whenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    AntResolver antResolver = new AntResolver();
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> antResolver.resolve(extension, new Project()));
  }

  /**
   * Test {@link AntResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_thenThrowBuildException() throws BuildException {
    // Arrange
    AntResolver antResolver = new AntResolver();
    antResolver.setAntfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> antResolver.resolve(extension, new Project()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link AntResolver}
   *   <li>{@link AntResolver#setAntfile(File)}
   *   <li>{@link AntResolver#setDestfile(File)}
   *   <li>{@link AntResolver#setTarget(String)}
   *   <li>{@link AntResolver#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    AntResolver actualAntResolver = new AntResolver();
    actualAntResolver.setAntfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualAntResolver.setDestfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualAntResolver.setTarget("Target");
    String actualToStringResult = actualAntResolver.toString();

    // Assert
    String toStringResult = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString();
    assertEquals(String.join("", "Ant[", toStringResult, "==>",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "]"), actualToStringResult);
  }
}
