package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import org.apache.tools.ant.helper.ProjectHelper2;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class ProjectHelperRepositoryDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Method under test: {@link ProjectHelperRepository#getInstance()}
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ProjectHelperRepository actualInstance = ProjectHelperRepository.getInstance();

    // Assert
    assertSame(actualInstance, actualInstance.getInstance());
  }

  /**
   * Test {@link ProjectHelperRepository#registerProjectHelper(String)} with {@code helperClassName}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperRepository#registerProjectHelper(String)}
   */
  @Test
  public void testRegisterProjectHelperWithHelperClassName_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ProjectHelperRepository.getInstance().registerProjectHelper("Helper Class Name"));
  }

  /**
   * Test {@link ProjectHelperRepository#getProjectHelperForBuildFile(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelperRepository#getProjectHelperForBuildFile(Resource)}
   */
  @Test
  public void testGetProjectHelperForBuildFile() throws BuildException {
    // Arrange
    ProjectHelperRepository instance = ProjectHelperRepository.getInstance();

    // Act
    ProjectHelper actualProjectHelperForBuildFile = instance.getProjectHelperForBuildFile(new Resource());

    // Assert
    assertTrue(actualProjectHelperForBuildFile instanceof ProjectHelper2);
    assertTrue(actualProjectHelperForBuildFile.getExtensionStack().isEmpty());
    assertTrue(actualProjectHelperForBuildFile.getImportStack().isEmpty());
    assertEquals(Main.DEFAULT_BUILD_FILENAME, actualProjectHelperForBuildFile.getDefaultBuildFile());
  }

  /**
   * Test {@link ProjectHelperRepository#getProjectHelperForAntlib(Resource)}.
   * <p>
   * Method under test: {@link ProjectHelperRepository#getProjectHelperForAntlib(Resource)}
   */
  @Test
  public void testGetProjectHelperForAntlib() throws BuildException {
    // Arrange
    ProjectHelperRepository instance = ProjectHelperRepository.getInstance();

    // Act
    ProjectHelper actualProjectHelperForAntlib = instance.getProjectHelperForAntlib(new Resource());

    // Assert
    assertTrue(actualProjectHelperForAntlib instanceof ProjectHelper2);
    assertTrue(actualProjectHelperForAntlib.getExtensionStack().isEmpty());
    assertTrue(actualProjectHelperForAntlib.getImportStack().isEmpty());
    assertEquals(Main.DEFAULT_BUILD_FILENAME, actualProjectHelperForAntlib.getDefaultBuildFile());
  }

  /**
   * Test {@link ProjectHelperRepository#getHelpers()}.
   * <p>
   * Method under test: {@link ProjectHelperRepository#getHelpers()}
   */
  @Test
  public void testGetHelpers() {
    // Arrange and Act
    Iterator<ProjectHelper> actualHelpers = ProjectHelperRepository.getInstance().getHelpers();

    // Assert
    ProjectHelper nextResult = actualHelpers.next();
    assertTrue(nextResult instanceof ProjectHelper2);
    assertFalse(actualHelpers.hasNext());
    assertTrue(nextResult.getExtensionStack().isEmpty());
    assertTrue(nextResult.getImportStack().isEmpty());
    assertEquals(Main.DEFAULT_BUILD_FILENAME, nextResult.getDefaultBuildFile());
  }
}
