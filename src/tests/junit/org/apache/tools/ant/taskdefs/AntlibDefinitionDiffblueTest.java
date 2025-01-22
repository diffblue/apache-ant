package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class AntlibDefinitionDiffblueTest {
  /**
   * Test {@link AntlibDefinition#setURI(String)}.
   * <ul>
   *   <li>When {@code ant:}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntlibDefinition#setURI(String)}
   */
  @Test
  public void testSetURI_whenAnt_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new AntlibDefinition()).setURI("ant:"));
  }

  /**
   * Test {@link AntlibDefinition#setURI(String)}.
   * <ul>
   *   <li>When {@code antlib:org.apache.tools.ant}.</li>
   *   <li>Then {@link AntlibDefinition} (default constructor) URI is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntlibDefinition#setURI(String)}
   */
  @Test
  public void testSetURI_whenAntlibOrgApacheToolsAnt_thenAntlibDefinitionUriIsEmptyString() throws BuildException {
    // Arrange
    AntlibDefinition antlibDefinition = new AntlibDefinition();

    // Act
    antlibDefinition.setURI("antlib:org.apache.tools.ant");

    // Assert that nothing has changed
    assertEquals("", antlibDefinition.getURI());
  }

  /**
   * Test {@link AntlibDefinition#setURI(String)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then {@link AntlibDefinition} (default constructor) URI is {@code Uri}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AntlibDefinition#setURI(String)}
   */
  @Test
  public void testSetURI_whenUri_thenAntlibDefinitionUriIsUri() throws BuildException {
    // Arrange
    AntlibDefinition antlibDefinition = new AntlibDefinition();

    // Act
    antlibDefinition.setURI("Uri");

    // Assert
    assertEquals("Uri", antlibDefinition.getURI());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link AntlibDefinition}
   *   <li>{@link AntlibDefinition#setAntlibClassLoader(ClassLoader)}
   *   <li>{@link AntlibDefinition#getAntlibClassLoader()}
   *   <li>{@link AntlibDefinition#getURI()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    AntlibDefinition actualAntlibDefinition = new AntlibDefinition();
    AntClassLoader classLoader = new AntClassLoader();
    actualAntlibDefinition.setAntlibClassLoader(classLoader);
    ClassLoader actualAntlibClassLoader = actualAntlibDefinition.getAntlibClassLoader();

    // Assert
    assertEquals("", actualAntlibDefinition.getURI());
    assertNotNull(actualAntlibClassLoader);
    Location location = actualAntlibDefinition.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAntlibDefinition.getDescription());
    assertNull(actualAntlibDefinition.getTaskName());
    assertNull(actualAntlibDefinition.getTaskType());
    assertNull(actualAntlibDefinition.getProject());
    assertNull(actualAntlibDefinition.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertSame(classLoader, actualAntlibClassLoader);
  }
}
