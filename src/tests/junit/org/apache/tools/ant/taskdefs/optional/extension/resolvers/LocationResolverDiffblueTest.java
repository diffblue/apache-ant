package org.apache.tools.ant.taskdefs.optional.extension.resolvers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.extension.Extension;
import org.junit.Test;

public class LocationResolverDiffblueTest {
  /**
   * Test {@link LocationResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link LocationResolver} (default constructor) Location is {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocationResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenLocationResolverLocationIsDotDot_thenReturnNameIsDownloads() throws BuildException {
    // Arrange
    LocationResolver locationResolver = new LocationResolver();
    locationResolver.setLocation("..");
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    File actualResolveResult = locationResolver.resolve(extension, new Project());

    // Assert
    assertEquals("Downloads", actualResolveResult.getName());
    assertTrue(actualResolveResult.isAbsolute());
  }

  /**
   * Test {@link LocationResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link LocationResolver} (default constructor) Location is {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocationResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenLocationResolverLocationIsDot_thenReturnNameIsApacheAnt11015() throws BuildException {
    // Arrange
    LocationResolver locationResolver = new LocationResolver();
    locationResolver.setLocation(".");
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    File actualResolveResult = locationResolver.resolve(extension, new Project());

    // Assert
    assertEquals("apache-ant-1.10.15", actualResolveResult.getName());
    assertTrue(actualResolveResult.isAbsolute());
  }

  /**
   * Test {@link LocationResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link LocationResolver} (default constructor) Location is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocationResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenLocationResolverLocationIsEmptyString() throws BuildException {
    // Arrange
    LocationResolver locationResolver = new LocationResolver();
    locationResolver.setLocation("");
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    File actualResolveResult = locationResolver.resolve(extension, new Project());

    // Assert
    assertEquals("apache-ant-1.10.15", actualResolveResult.getName());
    assertTrue(actualResolveResult.isAbsolute());
  }

  /**
   * Test {@link LocationResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link LocationResolver} (default constructor) Location is {@code foo}.</li>
   *   <li>Then return Name is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocationResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenLocationResolverLocationIsFoo_thenReturnNameIsFoo() throws BuildException {
    // Arrange
    LocationResolver locationResolver = new LocationResolver();
    locationResolver.setLocation("foo");
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    File actualResolveResult = locationResolver.resolve(extension, new Project());

    // Assert
    assertEquals("foo", actualResolveResult.getName());
    assertTrue(actualResolveResult.isAbsolute());
  }

  /**
   * Test {@link LocationResolver#resolve(Extension, Project)}.
   * <ul>
   *   <li>Given {@link LocationResolver} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocationResolver#resolve(Extension, Project)}
   */
  @Test
  public void testResolve_givenLocationResolver_thenThrowBuildException() throws BuildException {
    // Arrange
    LocationResolver locationResolver = new LocationResolver();
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertThrows(BuildException.class, () -> locationResolver.resolve(extension, new Project()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link LocationResolver}
   *   <li>{@link LocationResolver#setLocation(String)}
   *   <li>{@link LocationResolver#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    LocationResolver actualLocationResolver = new LocationResolver();
    actualLocationResolver.setLocation("Location");

    // Assert
    assertEquals("Location[Location]", actualLocationResolver.toString());
  }
}
