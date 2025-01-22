package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class LoaderUtilsDiffblueTest {
  /**
   * Test {@link LoaderUtils#getClassSource(Class)}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getClassSource(Class)}
   */
  @Test
  public void testGetClassSource_whenJavaLangObject_thenReturnNull() {
    // Arrange
    Class<Object> c = Object.class;

    // Act and Assert
    assertNull(LoaderUtils.getClassSource(c));
  }

  /**
   * Test {@link LoaderUtils#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_givenJavaLangObject_whenAntClassLoaderProjectIsProject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Couldn't load Resource ", typeClass);

    AntClassLoader c = new AntClassLoader();
    c.setProject(project);

    // Act and Assert
    assertNull(LoaderUtils.getResourceSource(c, "Resource"));
  }

  /**
   * Test {@link LoaderUtils#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_givenProject_whenAntClassLoaderProjectIsProject() {
    // Arrange
    AntClassLoader c = new AntClassLoader();
    c.setProject(new Project());

    // Act and Assert
    assertNull(LoaderUtils.getResourceSource(c, "Resource"));
  }

  /**
   * Test {@link LoaderUtils#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenAntClassLoader_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(LoaderUtils.getResourceSource(new AntClassLoader(), "Resource"));
  }

  /**
   * Test {@link LoaderUtils#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenEmptyString_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(LoaderUtils.getResourceSource(new AntClassLoader(), ""));
  }

  /**
   * Test {@link LoaderUtils#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(LoaderUtils.getResourceSource(null, "Resource"));
  }

  /**
   * Test {@link LoaderUtils#classNameToResource(String)}.
   * <p>
   * Method under test: {@link LoaderUtils#classNameToResource(String)}
   */
  @Test
  public void testClassNameToResource() {
    // Arrange, Act and Assert
    assertEquals("Class Name.class", LoaderUtils.classNameToResource("Class Name"));
  }

  /**
   * Test {@link LoaderUtils#classExists(ClassLoader, String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
   */
  @Test
  public void testClassExists_givenJavaLangObject_whenAntClassLoaderProjectIsProject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(".class", typeClass);

    AntClassLoader loader = new AntClassLoader();
    loader.setProject(project);

    // Act and Assert
    assertFalse(LoaderUtils.classExists(loader, "Class Name"));
  }

  /**
   * Test {@link LoaderUtils#classExists(ClassLoader, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
   */
  @Test
  public void testClassExists_givenProject_whenAntClassLoaderProjectIsProject_thenReturnFalse() {
    // Arrange
    AntClassLoader loader = new AntClassLoader();
    loader.setProject(new Project());

    // Act and Assert
    assertFalse(LoaderUtils.classExists(loader, "Class Name"));
  }

  /**
   * Test {@link LoaderUtils#classExists(ClassLoader, String)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LoaderUtils#classExists(ClassLoader, String)}
   */
  @Test
  public void testClassExists_whenAntClassLoader_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(LoaderUtils.classExists(new AntClassLoader(), "Class Name"));
  }
}
