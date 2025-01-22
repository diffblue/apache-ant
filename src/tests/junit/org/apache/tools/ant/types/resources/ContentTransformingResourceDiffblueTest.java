package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ContentTransformingResourceDiffblueTest {
  /**
   * Test {@link ContentTransformingResource#as(Class)}.
   * <ul>
   *   <li>Then return {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContentTransformingResource#as(Class)}
   */
  @Test
  public void testAs_thenReturnFileResource() throws BuildException {
    // Arrange
    FileResource other = new FileResource();

    BZip2Resource bZip2Resource = new BZip2Resource(other);
    Stack<Object> stack = new Stack<>();
    bZip2Resource.dieOnCircularReference(stack, new Project());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(other, bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ContentTransformingResource#as(Class)}.
   * <ul>
   *   <li>When {@code Appendable}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContentTransformingResource#as(Class)}
   */
  @Test
  public void testAs_whenOrgApacheToolsAntTypesResourcesAppendable_thenReturnNull() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();
    Class<Appendable> clazz = Appendable.class;

    // Act and Assert
    assertNull(bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ContentTransformingResource#as(Class)}.
   * <ul>
   *   <li>When {@code FileProvider}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ContentTransformingResource#as(Class)}
   */
  @Test
  public void testAs_whenOrgApacheToolsAntTypesResourcesFileProvider_thenReturnNull() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();
    Class<FileProvider> clazz = FileProvider.class;

    // Act and Assert
    assertNull(bZip2Resource.as(clazz));
  }

  /**
   * Test {@link ContentTransformingResource#isAppendSupported()}.
   * <p>
   * Method under test: {@link ContentTransformingResource#isAppendSupported()}
   */
  @Test
  public void testIsAppendSupported() {
    // Arrange, Act and Assert
    assertFalse((new BZip2Resource()).isAppendSupported());
  }
}
