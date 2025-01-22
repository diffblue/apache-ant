package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CCRmtypeDiffblueTest {
  /**
   * Test {@link CCRmtype#execute()}.
   * <ul>
   *   <li>Given {@link CCRmtype} (default constructor) TypeName is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCRmtype#execute()}
   */
  @Test
  public void testExecute_givenCCRmtypeTypeNameIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    CCRmtype ccRmtype = new CCRmtype();
    ccRmtype.setTypeKind("foo");
    ccRmtype.setTypeName(null);
    ccRmtype.setFailOnErr(false);
    ccRmtype.setComment(null);
    ccRmtype.setIgnore(false);
    ccRmtype.setRmAll(false);
    ccRmtype.setCommentFile(null);
    ccRmtype.setVOB(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccRmtype.execute());
  }

  /**
   * Test {@link CCRmtype#execute()}.
   * <ul>
   *   <li>Given {@link CCRmtype} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCRmtype#execute()}
   */
  @Test
  public void testExecute_givenCCRmtype_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CCRmtype()).execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCRmtype#setComment(String)}
   *   <li>{@link CCRmtype#setCommentFile(String)}
   *   <li>{@link CCRmtype#setIgnore(boolean)}
   *   <li>{@link CCRmtype#setRmAll(boolean)}
   *   <li>{@link CCRmtype#setTypeKind(String)}
   *   <li>{@link CCRmtype#setTypeName(String)}
   *   <li>{@link CCRmtype#setVOB(String)}
   *   <li>{@link CCRmtype#getComment()}
   *   <li>{@link CCRmtype#getCommentFile()}
   *   <li>{@link CCRmtype#getIgnore()}
   *   <li>{@link CCRmtype#getRmAll()}
   *   <li>{@link CCRmtype#getTypeKind()}
   *   <li>{@link CCRmtype#getTypeName()}
   *   <li>{@link CCRmtype#getVOB()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCRmtype ccRmtype = new CCRmtype();

    // Act
    ccRmtype.setComment("Comment");
    ccRmtype.setCommentFile("Cfile");
    ccRmtype.setIgnore(true);
    ccRmtype.setRmAll(true);
    ccRmtype.setTypeKind("Tk");
    ccRmtype.setTypeName("Tn");
    ccRmtype.setVOB("Vob");
    String actualComment = ccRmtype.getComment();
    String actualCommentFile = ccRmtype.getCommentFile();
    boolean actualIgnore = ccRmtype.getIgnore();
    boolean actualRmAll = ccRmtype.getRmAll();
    String actualTypeKind = ccRmtype.getTypeKind();
    String actualTypeName = ccRmtype.getTypeName();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Tk", actualTypeKind);
    assertEquals("Tn", actualTypeName);
    assertEquals("Vob", ccRmtype.getVOB());
    assertTrue(actualIgnore);
    assertTrue(actualRmAll);
  }

  /**
   * Test new {@link CCRmtype} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCRmtype}
   */
  @Test
  public void testNewCCRmtype() {
    // Arrange and Act
    CCRmtype actualCcRmtype = new CCRmtype();

    // Assert
    assertEquals("cleartool", actualCcRmtype.getClearToolCommand());
    assertNull(actualCcRmtype.getDescription());
    assertNull(actualCcRmtype.getTaskName());
    assertNull(actualCcRmtype.getTaskType());
    assertNull(actualCcRmtype.getComment());
    assertNull(actualCcRmtype.getCommentFile());
    assertNull(actualCcRmtype.getTypeKind());
    assertNull(actualCcRmtype.getTypeName());
    assertNull(actualCcRmtype.getVOB());
    assertNull(actualCcRmtype.getObjSelect());
    assertNull(actualCcRmtype.getViewPath());
    assertNull(actualCcRmtype.getProject());
    assertNull(actualCcRmtype.getOwningTarget());
    assertFalse(actualCcRmtype.getIgnore());
    assertFalse(actualCcRmtype.getRmAll());
    assertTrue(actualCcRmtype.getFailOnErr());
  }
}
