package org.apache.tools.ant.taskdefs.optional.pvcs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class PvcsDiffblueTest {
  /**
   * Test new {@link Pvcs} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Pvcs}
   */
  @Test
  public void testNewPvcs() {
    // Arrange and Act
    Pvcs actualPvcs = new Pvcs();

    // Assert
    assertEquals("\"P:", actualPvcs.getLineStart());
    assertEquals("{0}-arc({1})", actualPvcs.getFilenameFormat());
    assertNull(actualPvcs.getDescription());
    assertNull(actualPvcs.getTaskName());
    assertNull(actualPvcs.getTaskType());
    assertNull(actualPvcs.getConfig());
    assertNull(actualPvcs.getForce());
    assertNull(actualPvcs.getLabel());
    assertNull(actualPvcs.getPromotiongroup());
    assertNull(actualPvcs.getPvcsbin());
    assertNull(actualPvcs.getPvcsproject());
    assertNull(actualPvcs.getRepository());
    assertNull(actualPvcs.getRevision());
    assertNull(actualPvcs.getUserId());
    assertNull(actualPvcs.getWorkspace());
    assertNull(actualPvcs.getProject());
    assertNull(actualPvcs.getOwningTarget());
    assertFalse(actualPvcs.getIgnoreReturnCode());
    assertFalse(actualPvcs.getUpdateOnly());
    assertTrue(actualPvcs.getPvcsprojects().isEmpty());
  }

  /**
   * Test {@link Pvcs#execute()}.
   * <ul>
   *   <li>Given {@link PvcsProject} (default constructor) Name is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pvcs#execute()}
   */
  @Test
  public void testExecute_givenPvcsProjectNameIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    PvcsProject p = new PvcsProject();
    p.setName("");

    Pvcs pvcs = new Pvcs();
    pvcs.addPvcsproject(p);
    pvcs.setRepository("Required argument repository not specified");

    // Act and Assert
    assertThrows(BuildException.class, () -> pvcs.execute());
  }

  /**
   * Test {@link Pvcs#execute()}.
   * <ul>
   *   <li>Given {@link PvcsProject} (default constructor) Name is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pvcs#execute()}
   */
  @Test
  public void testExecute_givenPvcsProjectNameIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    PvcsProject p = new PvcsProject();
    p.setName(null);

    Pvcs pvcs = new Pvcs();
    pvcs.addPvcsproject(p);
    pvcs.setRepository("Required argument repository not specified");

    // Act and Assert
    assertThrows(BuildException.class, () -> pvcs.execute());
  }

  /**
   * Test {@link Pvcs#execute()}.
   * <ul>
   *   <li>Given {@link Pvcs} (default constructor) Repository is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pvcs#execute()}
   */
  @Test
  public void testExecute_givenPvcsRepositoryIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    Pvcs pvcs = new Pvcs();
    pvcs.setRepository("");

    // Act and Assert
    assertThrows(BuildException.class, () -> pvcs.execute());
  }

  /**
   * Test {@link Pvcs#execute()}.
   * <ul>
   *   <li>Given {@link Pvcs} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pvcs#execute()}
   */
  @Test
  public void testExecute_givenPvcs_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Pvcs()).execute());
  }

  /**
   * Test {@link Pvcs#addPvcsproject(PvcsProject)}.
   * <p>
   * Method under test: {@link Pvcs#addPvcsproject(PvcsProject)}
   */
  @Test
  public void testAddPvcsproject() {
    // Arrange
    Pvcs pvcs = new Pvcs();

    PvcsProject p = new PvcsProject();
    p.setName("Name");

    // Act
    pvcs.addPvcsproject(p);

    // Assert
    Vector<PvcsProject> pvcsprojects = pvcs.getPvcsprojects();
    assertEquals(1, pvcsprojects.size());
    assertSame(p, pvcsprojects.get(0));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Pvcs#setFilenameFormat(String)}
   *   <li>{@link Pvcs#setIgnoreReturnCode(boolean)}
   *   <li>{@link Pvcs#setLabel(String)}
   *   <li>{@link Pvcs#setLineStart(String)}
   *   <li>{@link Pvcs#setPromotiongroup(String)}
   *   <li>{@link Pvcs#setPvcsbin(String)}
   *   <li>{@link Pvcs#setPvcsproject(String)}
   *   <li>{@link Pvcs#setRepository(String)}
   *   <li>{@link Pvcs#setRevision(String)}
   *   <li>{@link Pvcs#setUpdateOnly(boolean)}
   *   <li>{@link Pvcs#setUserId(String)}
   *   <li>{@link Pvcs#setWorkspace(String)}
   *   <li>{@link Pvcs#getConfig()}
   *   <li>{@link Pvcs#getFilenameFormat()}
   *   <li>{@link Pvcs#getForce()}
   *   <li>{@link Pvcs#getIgnoreReturnCode()}
   *   <li>{@link Pvcs#getLabel()}
   *   <li>{@link Pvcs#getLineStart()}
   *   <li>{@link Pvcs#getPromotiongroup()}
   *   <li>{@link Pvcs#getPvcsbin()}
   *   <li>{@link Pvcs#getPvcsproject()}
   *   <li>{@link Pvcs#getPvcsprojects()}
   *   <li>{@link Pvcs#getRepository()}
   *   <li>{@link Pvcs#getRevision()}
   *   <li>{@link Pvcs#getUpdateOnly()}
   *   <li>{@link Pvcs#getUserId()}
   *   <li>{@link Pvcs#getWorkspace()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Pvcs pvcs = new Pvcs();

    // Act
    pvcs.setFilenameFormat("foo");
    pvcs.setIgnoreReturnCode(true);
    pvcs.setLabel("foo");
    pvcs.setLineStart("foo");
    pvcs.setPromotiongroup("foo");
    pvcs.setPvcsbin("Bin");
    pvcs.setPvcsproject("Prj");
    pvcs.setRepository("Repo");
    pvcs.setRevision("foo");
    pvcs.setUpdateOnly(true);
    pvcs.setUserId("foo");
    pvcs.setWorkspace("Ws");
    String actualConfig = pvcs.getConfig();
    String actualFilenameFormat = pvcs.getFilenameFormat();
    String actualForce = pvcs.getForce();
    boolean actualIgnoreReturnCode = pvcs.getIgnoreReturnCode();
    String actualLabel = pvcs.getLabel();
    String actualLineStart = pvcs.getLineStart();
    String actualPromotiongroup = pvcs.getPromotiongroup();
    String actualPvcsbin = pvcs.getPvcsbin();
    String actualPvcsproject = pvcs.getPvcsproject();
    Vector<PvcsProject> actualPvcsprojects = pvcs.getPvcsprojects();
    String actualRepository = pvcs.getRepository();
    String actualRevision = pvcs.getRevision();
    boolean actualUpdateOnly = pvcs.getUpdateOnly();
    String actualUserId = pvcs.getUserId();

    // Assert
    assertEquals("Bin", actualPvcsbin);
    assertEquals("Prj", actualPvcsproject);
    assertEquals("Repo", actualRepository);
    assertEquals("Ws", pvcs.getWorkspace());
    assertEquals("foo", actualFilenameFormat);
    assertEquals("foo", actualLabel);
    assertEquals("foo", actualLineStart);
    assertEquals("foo", actualPromotiongroup);
    assertEquals("foo", actualRevision);
    assertEquals("foo", actualUserId);
    assertNull(actualConfig);
    assertNull(actualForce);
    assertTrue(actualPvcsprojects.isEmpty());
    assertTrue(actualIgnoreReturnCode);
    assertTrue(actualUpdateOnly);
  }
}
