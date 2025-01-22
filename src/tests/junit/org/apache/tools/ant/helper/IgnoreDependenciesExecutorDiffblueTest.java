package org.apache.tools.ant.helper;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class IgnoreDependenciesExecutorDiffblueTest {
  /**
   * Test {@link IgnoreDependenciesExecutor#executeTargets(Project, String[])}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Project} (default constructor) KeepGoingMode is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IgnoreDependenciesExecutor#executeTargets(Project, String[])}
   */
  @Test
  public void testExecuteTargets_givenTrue_whenProjectKeepGoingModeIsTrue() throws BuildException {
    // Arrange
    IgnoreDependenciesExecutor ignoreDependenciesExecutor = new IgnoreDependenciesExecutor();

    Project project = new Project();
    project.setKeepGoingMode(true);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ignoreDependenciesExecutor.executeTargets(project, new String[]{"Target Names"}));
  }

  /**
   * Test {@link IgnoreDependenciesExecutor#executeTargets(Project, String[])}.
   * <ul>
   *   <li>When array of {@link String} with {@code Target Names}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IgnoreDependenciesExecutor#executeTargets(Project, String[])}
   */
  @Test
  public void testExecuteTargets_whenArrayOfStringWithTargetNames_thenThrowBuildException() throws BuildException {
    // Arrange
    IgnoreDependenciesExecutor ignoreDependenciesExecutor = new IgnoreDependenciesExecutor();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ignoreDependenciesExecutor.executeTargets(new Project(), new String[]{"Target Names"}));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link IgnoreDependenciesExecutor}
   *   <li>{@link IgnoreDependenciesExecutor#getSubProjectExecutor()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertTrue((new IgnoreDependenciesExecutor()).getSubProjectExecutor() instanceof SingleCheckExecutor);
  }
}
