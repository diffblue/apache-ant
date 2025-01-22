package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertThrows;
import java.io.IOException;
import org.apache.tools.ant.AntClassLoader;
import org.junit.Test;

public class JavaConstantResourceDiffblueTest {
  /**
   * Test {@link JavaConstantResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor) Name is {@code Attribute 'name' must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaConstantResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenJavaConstantResourceNameIsAttributeNameMustBeSet() throws IOException {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setName("Attribute 'name' must be set.");

    // Act and Assert
    assertThrows(IOException.class, () -> javaConstantResource.openInputStream(new AntClassLoader()));
  }

  /**
   * Test {@link JavaConstantResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor) Name is {@code Finding class}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaConstantResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenJavaConstantResourceNameIsFindingClass() throws IOException {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setName("Finding class ");

    // Act and Assert
    assertThrows(IOException.class, () -> javaConstantResource.openInputStream(new AntClassLoader()));
  }

  /**
   * Test {@link JavaConstantResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaConstantResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenJavaConstantResource_whenAntClassLoader() throws IOException {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act and Assert
    assertThrows(IOException.class, () -> javaConstantResource.openInputStream(new AntClassLoader()));
  }

  /**
   * Test {@link JavaConstantResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaConstantResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_whenNull() throws IOException {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setName("Attribute 'name' must be set.");

    // Act and Assert
    assertThrows(IOException.class, () -> javaConstantResource.openInputStream(null));
  }
}
