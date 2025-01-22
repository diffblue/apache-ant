package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CCMklbtypeDiffblueTest {
  /**
   * Test {@link CCMklbtype#execute()}.
   * <ul>
   *   <li>Given {@link CCMklbtype} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMklbtype#execute()}
   */
  @Test
  public void testExecute_givenCCMklbtype_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CCMklbtype()).execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMklbtype#setComment(String)}
   *   <li>{@link CCMklbtype#setCommentFile(String)}
   *   <li>{@link CCMklbtype#setGlobal(boolean)}
   *   <li>{@link CCMklbtype#setOrdinary(boolean)}
   *   <li>{@link CCMklbtype#setPbranch(boolean)}
   *   <li>{@link CCMklbtype#setReplace(boolean)}
   *   <li>{@link CCMklbtype#setShared(boolean)}
   *   <li>{@link CCMklbtype#setTypeName(String)}
   *   <li>{@link CCMklbtype#setVOB(String)}
   *   <li>{@link CCMklbtype#getComment()}
   *   <li>{@link CCMklbtype#getCommentFile()}
   *   <li>{@link CCMklbtype#getGlobal()}
   *   <li>{@link CCMklbtype#getOrdinary()}
   *   <li>{@link CCMklbtype#getPbranch()}
   *   <li>{@link CCMklbtype#getReplace()}
   *   <li>{@link CCMklbtype#getShared()}
   *   <li>{@link CCMklbtype#getTypeName()}
   *   <li>{@link CCMklbtype#getVOB()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMklbtype ccMklbtype = new CCMklbtype();

    // Act
    ccMklbtype.setComment("Comment");
    ccMklbtype.setCommentFile("Cfile");
    ccMklbtype.setGlobal(true);
    ccMklbtype.setOrdinary(true);
    ccMklbtype.setPbranch(true);
    ccMklbtype.setReplace(true);
    ccMklbtype.setShared(true);
    ccMklbtype.setTypeName("Tn");
    ccMklbtype.setVOB("Vob");
    String actualComment = ccMklbtype.getComment();
    String actualCommentFile = ccMklbtype.getCommentFile();
    boolean actualGlobal = ccMklbtype.getGlobal();
    boolean actualOrdinary = ccMklbtype.getOrdinary();
    boolean actualPbranch = ccMklbtype.getPbranch();
    boolean actualReplace = ccMklbtype.getReplace();
    boolean actualShared = ccMklbtype.getShared();
    String actualTypeName = ccMklbtype.getTypeName();

    // Assert
    assertEquals("Cfile", actualCommentFile);
    assertEquals("Comment", actualComment);
    assertEquals("Tn", actualTypeName);
    assertEquals("Vob", ccMklbtype.getVOB());
    assertTrue(actualGlobal);
    assertTrue(actualOrdinary);
    assertTrue(actualPbranch);
    assertTrue(actualReplace);
    assertTrue(actualShared);
  }

  /**
   * Test new {@link CCMklbtype} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMklbtype}
   */
  @Test
  public void testNewCCMklbtype() {
    // Arrange and Act
    CCMklbtype actualCcMklbtype = new CCMklbtype();

    // Assert
    assertEquals("cleartool", actualCcMklbtype.getClearToolCommand());
    assertNull(actualCcMklbtype.getDescription());
    assertNull(actualCcMklbtype.getTaskName());
    assertNull(actualCcMklbtype.getTaskType());
    assertNull(actualCcMklbtype.getComment());
    assertNull(actualCcMklbtype.getCommentFile());
    assertNull(actualCcMklbtype.getTypeName());
    assertNull(actualCcMklbtype.getVOB());
    assertNull(actualCcMklbtype.getObjSelect());
    assertNull(actualCcMklbtype.getViewPath());
    assertNull(actualCcMklbtype.getProject());
    assertNull(actualCcMklbtype.getOwningTarget());
    assertFalse(actualCcMklbtype.getGlobal());
    assertFalse(actualCcMklbtype.getPbranch());
    assertFalse(actualCcMklbtype.getReplace());
    assertFalse(actualCcMklbtype.getShared());
    assertTrue(actualCcMklbtype.getOrdinary());
    assertTrue(actualCcMklbtype.getFailOnErr());
  }
}
